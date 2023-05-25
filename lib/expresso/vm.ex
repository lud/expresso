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

  defp eval({:fun_call, _meta, [fun, args]} = fun_call, state) do
    __fun_impl__(fun, args, state)
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
end
