defmodule Expresso.PartialParserTest do
  alias Expresso.ParseError
  alias Expresso.Parser
  import Expresso.Parser
  import Expresso.Test.Util
  use ExUnit.Case, async: true

  defp run_parse(code, opts) do
    Expresso.parse(code, opts)
  end

  defp flush do
    flush([])
  end

  defp flush(acc) do
    receive do
      msg -> flush([msg | acc])
    after
      0 -> acc
    end
  end

  defp send_scopes(buf) do
    if (scope = buffer_scope(buf)) != [] do
      scope |> IO.inspect(label: ~S/scope/)
      send(self(), {:scope, scope})
    end
  end

  defp collect_scopes do
    _scopes =
      flush()
      #  |> Enum.uniq()
      |> Enum.map(fn {:scope, scope} -> scope end)
      |> IO.inspect(label: ~S/scopes/)
  end

  test "the parser can execute an action on EOI" do
    run_parse("",
      on_eoi: fn _ -> send(self(), :reached_eoi) end
    )

    assert_receive :reached_eoi
  end

  test "the parser will provide scopes that can be collected on EOI" do
    run_parse("aaa", on_eoi: &send_scopes/1)
    assert [variable: "aaa"] in collect_scopes()
  end

  test "EOI with get props" do
    run_parse("aaa.bbb", on_eoi: &send_scopes/1)
    assert [getprop: "bbb", variable: "aaa"] in collect_scopes()
  end

  test "EOI with get props within function call" do
    run_parse(~s/replace("hello", xxx.yyy, aaa.bbb/, on_eoi: &send_scopes/1)
    assert [getprop: "bbb", variable: "aaa"] in collect_scopes()
  end
end
