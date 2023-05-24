defmodule Expresso.Inerpreter do
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

  defp eval(int, state) when is_integer(int) do
    {int, state}
  end

  defp eval(float, state) when is_float(float) do
    {float, state}
  end

  defp eval({:dpath, _, path}, state) do
    {_value, _state} = fetch_value(state, path, path)
  end

  defp fetch_value(%{vars: vars} = state, path, full_path) do
    value = deref(vars, path, full_path)
    {value, state}
  end

  defp deref(kv, [k | path], full_path) when is_map_key(kv, k) do
    deref(Map.fetch!(kv, k), path, full_path)
  end

  defp deref(other, [k | path], full_path) do
    raise EvalError.invalid_path(full_path, other)
  end

  defp deref(v, [], _), do: v
end
