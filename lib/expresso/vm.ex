defmodule Expresso.VM do
  alias Expresso.EvalError

  @enable_debug false
  @enforce_keys [:vars, :scope, :debug]

  if @enable_debug do
    @dialyzer {:nowarn_function, print_debug: 2}
    @compiler {:inline, print_debug: 2, eval: 2}
  end

  defstruct vars: %{}, scope: [], debug: false

  def run(ast, input \\ %{}, opts \\ []) do
    vars =
      case is_map(input) do
        true -> input
        false -> %{}
      end

    {value, state} =
      eval(ast, %__MODULE__{vars: vars, scope: [], debug: Keyword.get(opts, :debug, false)})

    {:ok, value, state}
  rescue
    e in EvalError -> {:error, e}
  end

  # -- Evaluation of AST tokens -----------------------------------------------

  if @enable_debug do
    defp eval(ast, %{debug: true} = state) do
      debug(state, "EVAL", ast)
      do_eval(ast, state)
    end
  end

  defp eval(ast, state) do
    do_eval(ast, state)
  end

  defp do_eval({:literal, _, value}, state) do
    literal(value, state)
  end

  defp do_eval({:name, _, name} = var, state) do
    debug(state, "LOOKUP", name)
    debug(state, "STATE", state)
    {lookup_var!(state, var), state}
  end

  defp do_eval({:getprop, _, [subject, var]}, state) do
    {subject, state} = eval(subject, state)
    {deref!(subject, var), state}
  end

  defp do_eval({:fun_call, lc, [fun, args]} = fun_call, state) do
    __vm_function__(fun, args, state)
  catch
    {:arg_type_error, arg, arg_lc, arg_num, errmsg} ->
      raise EvalError.arg_type_error(fun_call, arg, arg_lc, arg_num, errmsg)

    :undefined_function_error ->
      raise EvalError.undefined_function_error(fun_call)

    {:argument_count_error, arg_num} ->
      raise EvalError.function_argument_count_error(fun, arg_num, lc)
  end

  defp do_eval({:lambda, _meta, [_, _]} = lambda, state) do
    build_lambda(lambda, state)
  end

  defp literal(int, state) when is_integer(int), do: {int, state}
  defp literal(float, state) when is_float(float), do: {float, state}
  defp literal(binary, state) when is_binary(binary), do: {binary, state}

  defp build_lambda({:lambda, _meta, [_, body]} = lambda, state) do
    snapshot = snapshot_scope(state, body)

    fun = fn args, state_in ->
      scope = Map.new(zip_args(lambda, args, 1))
      state_in = state_in |> put_scope(snapshot) |> put_scope(scope)
      {return, state_out} = eval(body, state_in)
      {return, state_out |> drop_scope(2)}
    end

    {{:lambda, fun}, state}
  end

  defp zip_args({:lambda, lc, [arg_vars, _body]}, args, n) do
    do_zip_args(arg_vars, args, n)
  catch
    :throw, {:missing_lambda_arg, k, n, arg_lc} ->
      raise EvalError.lambda_argument_count_error(k, n, arg_lc || lc)
  end

  defp do_zip_args([{:name, _, k} | ks], [v | vs], n), do: [{k, v} | do_zip_args(ks, vs, n + 1)]
  defp do_zip_args([], _, _), do: []
  defp do_zip_args([{:name, lc, k} | _], [], n), do: throw({:missing_lambda_arg, k, n, lc})

  # -- Reading values from scopes ---------------------------------------------

  defp lookup_var!(state, var) do
    state
    |> lookup_var(var)
    |> case do
      {:ok, value} -> value
      :error -> raise EvalError.key_error(var, state)
    end
  end

  defp lookup_var(%{vars: vars, scope: scopes}, var) do
    deref_scope(scopes, vars, var)
  end

  defp deref_scope([scope | _], _vars, {:name, _, k}) when is_map_key(scope, k) do
    {:ok, Map.fetch!(scope, k)}
  end

  defp deref_scope([_ | scopes], vars, var) do
    deref_scope(scopes, vars, var)
  end

  defp deref_scope([], vars, var) do
    deref(vars, var)
  end

  defp deref!(kv, var) do
    case deref(kv, var) do
      {:ok, value} -> value
      :error -> raise EvalError.key_error(var, kv)
    end
  end

  defp deref(kv, {:name, _, k}) when is_map_key(kv, k) do
    {:ok, Map.fetch!(kv, k)}
  end

  defp deref(_data, _var) do
    :error
  end

  defp put_scope(%{scope: scopes} = state, scope) do
    %{state | scope: [scope | scopes]}
  end

  defp drop_scope(%{scope: scope} = state, n) do
    %{state | scope: Enum.drop(scope, n)}
  end

  defp snapshot_scope(state, expression) do
    expression
    |> tree_names([])
    |> Enum.uniq()
    |> Enum.reduce([], fn {:name, _, key} = name, acc ->
      case lookup_var(state, name) do
        {:ok, value} -> [{key, value} | acc]
        :error -> acc
      end
    end)
    |> Map.new()
  end

  defp tree_names({:fun_call, _, [_, args]}, acc) do
    Enum.reduce(args, acc, &tree_names/2)
  end

  defp tree_names({:name, _, _} = name, acc) do
    [name | acc]
  end

  defp tree_names({:literal, _, _}, acc) do
    acc
  end

  defp tree_names({:lambda, _meta, [_args, body]}, acc) do
    # this is a sublambda.
    tree_names(body, acc)
  end

  defp tree_names({:getprop, _meta, [n1, n2]}, acc) do
    tree_names(n2, tree_names(n1, acc))
  end

  # -- Debugging --------------------------------------------------------------

  if @enable_debug do
    defp debug(%{debug: false}, _ast, _data) do
      nil
    end

    defp debug(%{debug: true}, ast, data) do
      print_debug(ast, data)
    end

    defp print_debug(tag, data) do
      IO.puts("#{tag} #{inspect(data)}")
    end
  else
    defp debug(_state, _ast, _data) do
      nil
    end
  end

  # -- Managing libraries of functions ----------------------------------------

  def library_info(module, key) do
    module.__vm_library__(key)
  end

  # -- Standard Library functions ---------------------------------------------

  # This module is itself defining a library module

  use Expresso.VM.Library

  # defp fun_impl("replace", args, state) do
  #   {subject, args, state} = pull_arg(args, state, &as_string/1, 1)
  #   {search, args, state} = pull_arg(args, state, &as_string/1, 2)
  #   {replacement, _args, state} = pull_arg(args, state, &as_string/1, 3)
  #   {String.replace(subject, search, replacement), state}
  # end

  function replace(subject :: as_string, search :: as_string, replacement :: as_string) do
    String.replace(subject, search, replacement)
  end

  function size(elem :: object), do: map_size(elem)

  function len(elem :: string | list) do
    case elem do
      str when is_binary(str) -> String.length(str)
      list when is_list(list) -> length(list)
    end
  end

  function add(a :: number, b :: number), do: a + b

  function join(str :: array(string), joiner :: string = "") do
    Enum.join(str, joiner)
  end

  function concat(str :: ~~~string) do
    Enum.join(str, "")
  end

  function equals(a :: any, b :: any) do
    a == b
  end

  function dump(a :: any) do
    inspect(a)
  end

  defp __vm_function__("call", args, state) do
    {[fun, f_args], _, state} = pull_args(args, state, [:lambda, {:spread_type, :any}])
    {value, state} = fun.(f_args, state)
    {value, state}
  end

  defp __vm_function__("for_each", args, state) do
    {[list, fun], _, state} = pull_args(args, state, [:list, :lambda])

    list
    |> Enum.reduce({[], state}, fn elem, {values, state} ->
      {value, state} = fun.([elem], state)
      {[value | values], state}
    end)
    |> then(fn {values, state} -> {:lists.reverse(values), state} end)
  end

  defp __vm_function__("find", args, state) do
    {[list, fun], _, state} = pull_args(args, state, [:list, :lambda])

    list
    |> Enum.reduce_while({nil, state}, fn elem, {nil, state} ->
      {matched?, state} = fun.([elem], state)

      if matched?,
        do: {:halt, {elem, state}},
        else: {:cont, {nil, state}}
    end)
  end

  defp __vm_function__("reduce", raw_args, state) do
    {[list, init, fun], _args, state} =
      case raw_args do
        [_, _, _ | _] ->
          pull_args(raw_args, state, [:list, :any, :lambda])

        _ ->
          {[list, fun], args, state} = pull_args(raw_args, state, [:list, :lambda])

          case list do
            [first | rest] ->
              {[rest, first, fun], args, state}

            [] ->
              [{:name, lc, _} | _] = raw_args

              raise EvalError.empty_error(
                      lc,
                      "empty list given to `reduce` without initial value"
                    )
          end
      end

    list
    |> Enum.reduce({init, state}, fn elem, {acc, state} ->
      # ensure match error here
      {new_acc, state} = fun.([elem, acc], state)
      {new_acc, state}
    end)
  end
end
