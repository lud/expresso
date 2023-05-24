defmodule Expresso.VMTest do
  alias Expresso.EvalError
  alias Expresso.VM
  use ExUnit.Case, async: true
  import Expresso.Test.Util

  defp run(code, input \\ %{}) do
    tokens = get_tokens(code, print: true)
    assert {:ok, value, _state} = VM.run(tokens, input)
    value
  end

  defp get_error(code, input) do
    tokens = get_tokens(code)
    assert {:error, reason} = VM.run(tokens, input)
    assert is_struct(reason, EvalError)
    reason = EvalError.with_source(reason, code)
    message = Exception.message(reason)

    IO.puts([IO.ANSI.magenta(), message, IO.ANSI.reset()])

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

    # Errors when the data is not a map

    err = get_error("greeting.french", %{"greeting" => "hello"})
    # the error tells that a key was taken from what is not a map
    assert false == err.meta[:map?]
    # the error contains the full path
    assert "french" == err.meta[:key]
    assert Exception.message(err) =~ "cannot dereference key `french`"

    # same with quoted paths
    assert "hello" == run("greeting.'1'", %{"greeting" => %{"1" => "hello"}})
    err = get_error("greeting.'2'", %{"greeting" => "hello"})
    # the error tells that a key was taken from what is not a map
    assert false == err.meta[:map?]
    # the error contains the full path
    assert "2" == err.meta[:key]
    assert Exception.message(err) =~ "cannot dereference key `'2'`"

    # Errors when the data does not contain key

    err = get_error("greeting.french", %{"greeting" => %{"english" => "hello"}})

    assert true == err.meta[:map?]
    # the error contains the full path
    assert "french" == err.meta[:key]
    assert Exception.message(err) =~ "could not find key `french`"
  end

  test "evaluate a function call" do
    assert "he//o" = run(~s[replace(greetings, "l", "/")], %{"greetings" => "hello"})
  end

  # we want very user friendly functions
  test "the replace function accepts numbers as arguments" do
    assert "0000" = run(~s[replace(1111, 1, 0)])
  end

  test "the replace function cannot accept a map as argument" do
    err = get_error(~s[replace(greetings, "l", "/")], %{"greetings" => %{"english" => "hello"}})
    assert Exception.message(err) =~ "invalid 1st argument for function `replace`"

    err =
      get_error(~s[replace("hello", greetings, "/")], %{"greetings" => %{"english" => "hello"}})

    assert Exception.message(err) =~ "invalid 2nd argument for function `replace`"

    err =
      get_error(~s[replace("hello", "l", greetings)], %{"greetings" => %{"english" => "hello"}})

    assert Exception.message(err) =~ "invalid 3rd argument for function `replace`"

    # check that deref still works
    assert "he//o" = run(~s[replace("hello", "l", chars.slash)], %{"chars" => %{"slash" => "/"}})

    # nesting
    assert "he//o" =
             run(~s[replace(replace("hello", "l", chars.bar), chars.bar, chars.slash)], %{
               "chars" => %{"slash" => "/", "bar" => "|"}
             })

    # chaining
    assert "he//o" =
             run(
               ~s"""
                "hello"
                  :replace("l", chars.bar)
                  :replace(chars.bar, chars.slash)
               """,
               %{"chars" => %{"slash" => "/", "bar" => "|"}}
             )
  end
end
