defmodule Expresso.ParserTest do
  alias Expresso.ParseError
  alias Expresso.Parser
  use ExUnit.Case, async: true
  import Expresso.Test.Util

  defp to_tokens(code) do
    get_tokens(code, print: false)
  end

  defp get_tokens(code, opts \\ []) do
    print? = Keyword.pop(opts, :print, false)

    if print? do
      IO.puts([IO.ANSI.yellow(), code, IO.ANSI.reset()])
    end

    value_eps = Expresso.parse!(code)

    if print? do
      IO.puts([IO.ANSI.blue(), "=> ", inspect(value_eps), IO.ANSI.reset()])
    end

    value_eps
  end

  defp assert_literal(expected, tokens) do
    assert {:literal, _, ^expected} = tokens
  end

  test "can parse an integer" do
    assert_literal(1, to_tokens("1"))
    assert_literal(-1, to_tokens("-1"))
    assert_literal(123, to_tokens("123"))
    assert_literal(-123, to_tokens("-123"))
    assert_literal(-0, to_tokens("-0"))
  end

  test "can parse a float" do
    assert_literal(1.1, to_tokens("1.1"))
    assert_literal(1.0e10, to_tokens("1.0e10"))
    assert_literal(1.0e+10, to_tokens("1.0e+10"))
    assert_literal(1.0e-10, to_tokens("1.0e-10"))
    assert_literal(1.0e10, to_tokens("1.0E10"))
    assert_literal(1.0e+10, to_tokens("1.0E+10"))
    assert_literal(1.0e-10, to_tokens("1.0E-10"))
    assert_literal(-1.0e10, to_tokens("-1.0E10"))
    assert_literal(-1.0e+10, to_tokens("-1.0E+10"))
    assert_literal(-1.0e-10, to_tokens("-1.0E-10"))
  end

  test "can parse a data path" do
    assert {:name, [line: 0, column: 0], "a"} = to_tokens("a")
    assert {:name, [line: 0, column: 0], "my_var"} = to_tokens("my_var")

    assert {
             :getprop,
             _,
             [
               {:name, [line: 0, column: 0], "my_var"},
               {:name, [line: 0, column: 7], "my_sub"}
             ]
           } = to_tokens("my_var.my_sub")

    assert {
             :getprop,
             _,
             [
               {:getprop, _,
                [{:name, [line: 0, column: 0], "my_var"}, {:name, [line: 0, column: 7], "my_sub"}]},
               {:name, [line: 0, column: 14], "my_third"}
             ]
           } = to_tokens("my_var.my_sub.my_third")
  end

  test "can parse a data path with freeform keys" do
    assert {:name, [line: 0, column: 0], "1"} = to_tokens("'1'")

    assert {:getprop, _, [{:name, [line: 0, column: 0], "1"}, {:name, _, "2"}]} =
             to_tokens("'1'.'2'")
  end

  test "can parse a data path with mixed keys" do
    assert {:getprop, _, [{:name, [line: 0, column: 0], "a"}, {:name, [line: 0, column: 2], "1"}]} =
             to_tokens("a.'1'")

    assert {
             :getprop,
             _,
             [
               {:getprop, _,
                [
                  {:getprop, _,
                   [{:name, [line: 0, column: 0], "a"}, {:name, [line: 0, column: 2], "b"}]},
                  {:name, [line: 0, column: 4], "1"}
                ]},
               {:name, [line: 0, column: 8], "c"}
             ]
           } = to_tokens("a.b.'1'.c")
  end

  test "can parse a function call" do
    assert {:fun_call, _, ["call", [{:name, _, "some"}]]} = to_tokens("call(some)")

    assert {:fun_call, _, ["add", [{:literal, _, 1}, {:literal, _, 2}, {:literal, _, 3}]]} =
             to_tokens("add(1, 2, 3)")

    assert {:fun_call, _,
            [
              "add",
              [{:fun_call, _, ["add", [{:literal, _, 1}, {:literal, _, 2}]]}, {:literal, _, 3}]
            ]} = to_tokens("add(add(1, 2), 3)")
  end

  test "can parse a method call" do
    assert {:fun_call, _, ["add", [{:literal, _, 1}, {:literal, _, 2}]]} = to_tokens("1:add(2)")

    assert {:fun_call, _,
            [
              "add",
              [{:fun_call, _, ["add", [{:literal, _, 1}, {:literal, _, 2}]]}, {:literal, _, 3}]
            ]} = to_tokens("1:add(2):add(3)")

    assert {:fun_call, _,
            [
              "add",
              [
                {:fun_call, _, ["add", [{:literal, _, 1}, {:literal, _, 2}]]},
                {:name, _, "my_var"}
              ]
            ]} = to_tokens("1:add(2):add(my_var)")

    assert {:fun_call, _,
            [
              "add",
              [
                {:fun_call, _, ["add", [{:literal, _, 1}, {:literal, _, 2}]]},
                {:fun_call, _, ["add", [{:literal, _, 3}, {:literal, _, 4}]]}
              ]
            ]} = to_tokens("1:add(2):add(add(3,4))")

    assert {:fun_call, _, ["size", [{:name, _, "some_map"}]]} = to_tokens("some_map:size()")
  end

  test "parse errors are returned with an exception struct" do
    assert {:error, %ParseError{}} = Expresso.parse("!%//()z^q")
  end

  test "can parse a quoted string" do
    assert_literal(~s/hello/, to_tokens(~s/"hello"/))
    assert_literal(~s/with"escape/, to_tokens(~S/"with\"escape"/))
    assert_literal(~s/with"two"escapes/, to_tokens(~S/"with\"two\"escapes"/))
    assert_literal(~s/with""a mess ""/, to_tokens(~S/"with\"\"a mess \"\""/))

    assert_literal(
      ~s/with a\nnewline/,
      to_tokens(~S"""
      "with a
      newline"
      """)
    )
  end

  test "can parse a lambda expression" do
    assert {:fun_call, [line: 0, column: 10],
            ["map", [{:name, [line: 0, column: 0], "some_list"}, lambda]]} =
             to_tokens("some_list:map(fn(x) => x end)")

    assert {:lambda, _, [[{:arg, [line: 0, column: 17], "x"}], {:name, _, "x"}]} = lambda
  end
end
