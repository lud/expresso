defmodule Expresso.ParserTest do
  alias Expresso.Tokenizer
  alias Expresso.ParseError
  alias Expresso.Parser
  use ExUnit.Case, async: true

  defp to_ast(code) do
    assert {:ok, tokens} = Tokenizer.tokenize(code)

    case Parser.parse_tokens(tokens, debug: true) do
      {:ok, ast} ->
        ast

      {:error, %ParseError{} = e} ->
        IO.puts([
          IO.ANSI.yellow(),
          """
          CODE
          #{code}

          TOKENS
          #{inspect(tokens, pretty: true)}
          """,
          IO.ANSI.reset()
        ])

        Process.sleep(100)
        flunk(Exception.message(e))
    end
  end

  defp assert_literal(expected, tokens) do
    assert {:literal, _, ^expected} = tokens
  end

  test "can parse an integer" do
    assert_literal(1, to_ast("1"))
    assert_literal(-1, to_ast("-1"))
    assert_literal(123, to_ast("123"))
    assert_literal(-123, to_ast("-123"))
    assert_literal(-0, to_ast("-0"))
  end

  test "can parse a float" do
    assert_literal(1.1, to_ast("1.1"))
    assert_literal(1.0e10, to_ast("1.0e10"))
    assert_literal(1.0e+10, to_ast("1.0e+10"))
    assert_literal(1.0e-10, to_ast("1.0e-10"))
    assert_literal(1.0e10, to_ast("1.0E10"))
    assert_literal(1.0e+10, to_ast("1.0E+10"))
    assert_literal(1.0e-10, to_ast("1.0E-10"))
    assert_literal(-1.0e10, to_ast("-1.0E10"))
    assert_literal(-1.0e+10, to_ast("-1.0E+10"))
    assert_literal(-1.0e-10, to_ast("-1.0E-10"))
  end

  test "can parse a data path" do
    assert {:name, _, "a"} = to_ast("a")
    assert {:name, _, "my_var"} = to_ast("my_var")

    assert {
             :getprop,
             _,
             [
               {:name, _, "my_var"},
               {:name, _, "my_sub"}
             ]
           } = to_ast("my_var.my_sub")

    assert {
             :getprop,
             _,
             [
               {:getprop, _, [{:name, _, "my_var"}, {:name, _, "my_sub"}]},
               {:name, _, "my_third"}
             ]
           } = to_ast("my_var.my_sub.my_third")
  end

  test "can parse a data path with freeform keys" do
    assert {:name, _, "1"} = to_ast("'1'")

    assert {:getprop, _, [{:name, _, "1"}, {:name, _, "2"}]} = to_ast("'1'.'2'")
  end

  test "can parse a data path with mixed keys" do
    assert {:getprop, _, [{:name, _, "a"}, {:name, _, "1"}]} = to_ast("a.'1'")

    assert {
             :getprop,
             _,
             [
               {:getprop, _,
                [
                  {:getprop, _, [{:name, _, "a"}, {:name, _, "b"}]},
                  {:name, _, "1"}
                ]},
               {:name, _, "c"}
             ]
           } = to_ast("a.b.'1'.c")
  end

  test "can parse a function call" do
    assert {:fun_call, _, ["call", [{:name, _, "some"}]]} = to_ast("call(some)")

    assert {:fun_call, _, ["add", [{:literal, _, 1}, {:literal, _, 2}, {:literal, _, 3}]]} =
             to_ast("add(1, 2, 3)")

    assert {:fun_call, _,
            [
              "add",
              [{:fun_call, _, ["add", [{:literal, _, 1}, {:literal, _, 2}]]}, {:literal, _, 3}]
            ]} = to_ast("add(add(1, 2), 3)")
  end

  test "can parse a method call" do
    assert {:fun_call, _, ["add", [{:literal, _, 1}, {:literal, _, 2}]]} = to_ast("1:add(2)")

    assert {:fun_call, _,
            [
              "add",
              [{:fun_call, _, ["add", [{:literal, _, 1}, {:literal, _, 2}]]}, {:literal, _, 3}]
            ]} = to_ast("1:add(2):add(3)")

    assert {:fun_call, _,
            [
              "add",
              [
                {:fun_call, _, ["add", [{:literal, _, 1}, {:literal, _, 2}]]},
                {:name, _, "my_var"}
              ]
            ]} = to_ast("1:add(2):add(my_var)")

    assert {:fun_call, _,
            [
              "add",
              [
                {:fun_call, _, ["add", [{:literal, _, 1}, {:literal, _, 2}]]},
                {:fun_call, _, ["add", [{:literal, _, 3}, {:literal, _, 4}]]}
              ]
            ]} = to_ast("1:add(2):add(add(3,4))")

    assert {:fun_call, _, ["size", [{:name, _, "some_map"}]]} = to_ast("some_map:size()")
  end

  test "parse errors are returned with an exception struct" do
    # the final closing paren is missing
    code = "a(b(unfinished(a)), looong_arg"

    assert {:error, %ParseError{}} =
             code
             |> Expresso.tokenize!()
             |> Expresso.parse_tokens(debug: false)

    # a coma is missing
    code = "a(b(unfinished(a b)), looong_arg)"

    assert {:error, %ParseError{}} =
             code
             |> Expresso.tokenize!()
             |> Expresso.parse_tokens(debug: false)
  end

  test "can parse a quoted string" do
    assert_literal(~s/hello/, to_ast(~s/"hello"/))
    assert_literal(~s/with"escape/, to_ast(~S/"with\"escape"/))
    assert_literal(~s/with"two"escapes/, to_ast(~S/"with\"two\"escapes"/))
    assert_literal(~s/with""a mess ""/, to_ast(~S/"with\"\"a mess \"\""/))

    assert_literal(
      ~s/with a\nnewline/,
      to_ast(~S"""
      "with a
      newline"
      """)
    )
  end

  test "can parse a lambda expression" do
    assert {:fun_call, _, ["map", [{:name, _, "some_list"}, lambda]]} =
             to_ast("some_list:map(fn(x) => x end)")

    assert {:lambda, _, [[{:name, _, "x"}], {:name, _, "x"}]} = lambda
  end
end
