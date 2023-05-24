defmodule Expresso.VM do
  alias Expresso.EvalError
  @enforce_keys [:data, :vars, :scope]
  defstruct data: nil, vars: %{}, scope: []

  def run(ast, input \\ %{}) do
    vars =
      case is_map(input) do
        true -> input
        false -> %{}
      end

    {value, state} = eval(ast, %__MODULE__{vars: vars, data: input, scope: []})
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
    {value, state} = deref_root(state, var)
    {value, state}
  end

  defp eval({:getprop, _, [subject, var]}, state) do
    {subject, state} = eval(subject, state)
    {deref(subject, var), state}
  end

  defp eval({:fun_call, meta, [fun, args]} = fun_call, state) do
    fun_impl(fun, args, state)
  catch
    {:arg_error, arg, arg_lc, arg_num, errmsg} ->
      raise EvalError.arg_error(fun_call, arg, arg_lc, arg_num, errmsg)
  end

  defp literal(int, state) when is_integer(int), do: {int, state}
  defp literal(float, state) when is_float(float), do: {float, state}
  defp literal(binary, state) when is_binary(binary), do: {binary, state}

  # -- Reading values from scopes ---------------------------------------------

  defp deref_root(%{vars: vars} = state, var) do
    value = deref(vars, var)
    {value, state}
  end

  defp deref(kv, {:var, _, k}) when is_map_key(kv, k) do
    Map.fetch!(kv, k)
  end

  defp deref(data, var) do
    raise EvalError.key_error(var, data)
  end

  # -- Standard Library functions ---------------------------------------------

  defp fun_impl("replace", args, state) do
    {subject, args, state} = pull_arg(args, state, &loose_string/1, 1)
    {search, args, state} = pull_arg(args, state, &loose_string/1, 2)
    {replacement, _args, state} = pull_arg(args, state, &loose_string/1, 3)
    {String.replace(subject, search, replacement), state}
  end

  defp pull_arg([arg | args], state, caster, arg_num) do
    {arg_val, state} = eval(arg, state)

    case caster.(arg_val) do
      {:ok, value} ->
        {value, args, state}

      {:error, errmsg} ->
        lc = fetch_lc(arg)

        throw({:arg_error, arg, lc, arg_num, errmsg})
    end
  end

  defp loose_string(arg) when is_binary(arg), do: {:ok, arg}
  defp loose_string(arg) when is_integer(arg), do: {:ok, Integer.to_string(arg)}
  defp loose_string(arg) when is_float(arg), do: {:ok, Float.to_string(arg)}
  defp loose_string(arg), do: {:error, "cannot use #{typeof(arg)} as a string"}

  defp typeof(list) when is_list(list), do: "array"
  defp typeof(map) when is_map(map), do: "object"
  defp typeof(binary) when is_binary(binary), do: "binary"
  defp typeof(integer) when is_integer(integer), do: "integer"
  defp typeof(float) when is_float(float), do: "float"

  defp fetch_lc({_, lc, _}), do: lc
  defp fetch_lc(_), do: nil
end
