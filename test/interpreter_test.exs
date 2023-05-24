defmodule Expresso.InerpreterTest do
  alias Expresso.EvalError
  alias Expresso.ParseError
  alias Expresso.Inerpreter
  use ExUnit.Case, async: true
  import Expresso.Test.Util

  defp run(code, input \\ %{}) do
    tokens = get_tokens(code)
    assert {:ok, value, _state} = Inerpreter.run(tokens, input)
    value
  end

  defp get_error(code, input \\ %{}) do
    tokens = get_tokens(code)
    assert {:error, reason} = Inerpreter.run(tokens, input)
    assert is_struct(reason, Expresso.EvalError)
    message = Exception.message(reason)

    if String.contains?(message, "while retrieving Exception.message") do
      flunk("""
      No message clause for exception: #{inspect(reason, pretty: true)}
      """)
    end

    reason
  end

  test "evaluate a single value" do
    assert 1 == run("1")
    assert 1.1 == run("1.1")
    assert 1.0e+10 == run("1.0e+10")
  end

  test "evaluate a path" do
    assert "hello" == run("greeting", %{"greeting" => "hello"})
    assert "hello" == run("greeting.english", %{"greeting" => %{"english" => "hello"}})

    err = get_error("greeting.french", %{"greeting" => "hello"})
    # the error tells that a key was taken from what is not a map
    assert false == err.meta[:map?]
    # the error contains the full path
    assert ["greeting", "french"] == err.meta[:path]
    assert Exception.message(err) =~ "greeting.french"

    # same with quoted pathes
    assert "hello" == run("greeting.'1'", %{"greeting" => %{"1" => "hello"}})
    err = get_error("greeting.'2'", %{"greeting" => "hello"})
    # the error tells that a key was taken from what is not a map
    assert false == err.meta[:map?]
    # the error contains the full path
    assert ["greeting", "2"] == err.meta[:path]
    assert Exception.message(err) =~ "greeting.'2'"
  end
end
