defmodule Expresso.VM do
  alias Expresso.EvalError

  @enforce_keys [:vars, :scope]
  defstruct vars: %{}, scope: []

  def run(ast, input \\ %{}) do
    vars =
      case is_map(input) do
        true -> input
        false -> %{}
      end

    {value, state} = eval(ast, %__MODULE__{vars: vars, scope: []})
    {:ok, value, state}
    {:ok, value, state}
  rescue
    e in EvalError -> {:error, e}
  end

  # -- Evaluation of AST tokens -----------------------------------------------

  defp eval({:literal, _, value}, state) do
    literal(value, state)
  end

  # defp eval({:dpath, _, path}, state) do
  #   {_value, _state} = fetch_value(state, path, path)
  # end

  defp eval({:var, _, _} = var, state) do
    {value, state} = lookup_var(state, var)
    {value, state}
  end

  defp eval({:getprop, _, [subject, var]}, state) do
    {subject, state} = eval(subject, state)
    {deref(subject, var), state}
  end

  defp eval({:fun_call, _meta, [fun, args]} = fun_call, state) do
    __vm_function__(fun, args, state)
  catch
    {:arg_type_error, arg, arg_lc, arg_num, errmsg} ->
      raise EvalError.arg_type_error(fun_call, arg, arg_lc, arg_num, errmsg)

    :undefined_function_error ->
      raise EvalError.undefined_function_error(fun_call)
  end

  defp eval({:lambda, _meta, [_, _]} = lambda, state) do
    build_lambda(lambda, state)
  end

  defp literal(int, state) when is_integer(int), do: {int, state}
  defp literal(float, state) when is_float(float), do: {float, state}
  defp literal(binary, state) when is_binary(binary), do: {binary, state}

  defp build_lambda({:lambda, _meta, [_, body]} = lambda, state) do
    fun = fn args, state_in ->
      scope = Map.new(zip_args(lambda, args, 1))
      scope |> IO.inspect(label: ~S/scope/)
      state_in = put_scope(state_in, scope)
      {return, state_out} = eval(body, state_in)
      {return, drop_scope(state_out)}
    end

    {{:lambda, fun}, state}
  end

  defp zip_args({:lambda, lc, [arg_vars, _body]}, args, n) do
    do_zip_args(arg_vars, args, n)
  catch
    :throw, {:missing_lambda_arg, k, n, arg_lc} ->
      raise EvalError.argument_count_error(k, n, arg_lc || lc)
  end

  defp do_zip_args([{:var, _, k} | ks], [v | vs], n), do: [{k, v} | do_zip_args(ks, vs, n + 1)]
  defp do_zip_args([], _, _), do: []
  defp do_zip_args([{:var, lc, k} | _], [], n), do: throw({:missing_lambda_arg, k, n, lc})

  # -- Reading values from scopes ---------------------------------------------

  defp lookup_var(%{vars: vars, scope: scopes} = state, var) do
    vars |> IO.inspect(label: ~S/vars/)
    scopes |> IO.inspect(label: ~S/scopes/)
    value = deref_scope(scopes, vars, var)
    {value, state}
  end

  defp deref_scope([], vars, var) do
    deref(vars, var)
  end

  defp deref_scope([scope | _], _vars, {:var, _, k}) when is_map_key(scope, k) do
    Map.fetch!(scope, k)
  end

  defp deref_scope([_ | scopes], vars, var) do
    deref_scope(scopes, vars, var)
  end

  defp deref(kv, {:var, _, k}) when is_map_key(kv, k) do
    Map.fetch!(kv, k)
  end

  defp deref(data, var) do
    raise EvalError.key_error(var, data)
  end

  defp put_scope(%__MODULE__{scope: scopes} = state, scope) do
    %__MODULE__{state | scope: [scope | scopes]}
  end

  defp drop_scope(%__MODULE__{scope: [_ | scope]} = state) do
    %__MODULE__{state | scope: scope}
  end

  # -- Standard Library functions ---------------------------------------------

  use Expresso.VM.Library

  # defp fun_impl("replace", args, state) do
  #   {subject, args, state} = pull_arg(args, state, &loose_string/1, 1)
  #   {search, args, state} = pull_arg(args, state, &loose_string/1, 2)
  #   {replacement, _args, state} = pull_arg(args, state, &loose_string/1, 3)
  #   {String.replace(subject, search, replacement), state}
  # end

  function replace(subject :: loose_string, search :: loose_string, replacement :: loose_string) do
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

  defp __vm_function__("for_each", args, state) do
    {[list, fun], _, state} = pull_args(args, state, [&__vm_type__list/1, &__vm_type__lambda/1])

    list
    |> Enum.reduce({[], state}, fn elem, {values, state} ->
      {value, state} = fun.([elem], state)
      {[value | values], state}
    end)
    |> then(fn {values, state} -> {:lists.reverse(values), state} end)
  end

  defp __vm_function__("reduce", raw_args = args, state) do
    {[list, init, fun], args, state} =
      case args do
        [_, _, _ | _] ->
          pull_args(args, state, [&__vm_type__list/1, &__vm_type__any/1, &__vm_type__lambda/1])

        _ ->
          {[list, fun], args, state} =
            pull_args(args, state, [&__vm_type__list/1, &__vm_type__lambda/1])

          list |> IO.inspect(label: ~S/list/)

          case list do
            [first | rest] ->
              {[rest, first, fun], args, state}

            [] ->
              [{:var, lc, _} | _] = raw_args

              raise EvalError.empty_error(
                      lc,
                      "empty list given to `reduce` without initial value"
                    )
          end
      end

    list
    |> Enum.reduce({init, state}, fn elem, {acc, state} ->
      {new_acc, state} = fun.([elem, acc], state)
      {new_acc, state}
    end)
  end
end
