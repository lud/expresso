defmodule Expresso.ParseError do
  defexception [:reason, :line, :submessage]

  def message(%{line: l} = e) do
    "#{reason(e)} at line #{l}"
  end

  defp reason(%{reason: :generic, submessage: msg}),
    do: msg
end
