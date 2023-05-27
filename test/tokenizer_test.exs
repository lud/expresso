defmodule Expresso.TokenizerTest do
  alias Expresso.ParseError
  use ExUnit.Case, async: true

  defp to_tokens(code) do
    assert {:ok, tokens} = Expresso.Tokenizer.tokenize(code)

    tokens
  end

  defp assert_literal(expected, tokens) do
    assert [{:literal, _, ^expected}] = tokens
  end

  test "can tokenize an integer" do
    assert_literal(1, to_tokens("1"))
    assert_literal(-1, to_tokens("-1"))
    assert_literal(123, to_tokens("123"))
    assert_literal(-123, to_tokens("-123"))
    assert_literal(-0, to_tokens("-0"))
  end

  test "can tokenize a float" do
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

  test "can tokenize a data path" do
    assert [{:name, _, "a"}] = to_tokens("a")
    assert [{:name, _, "a"}] = to_tokens("   a   ")

    assert [{:name, _, "my_var"}] = to_tokens("my_var")

    assert [{:name, _, "my_var"}, :., {:name, _, "my_sub"}] = to_tokens("my_var.my_sub")

    assert [
             {:name, _, "my_var"},
             :.,
             {:name, _, "my_sub"},
             :.,
             {:name, _, "my_third"}
           ] = to_tokens("my_var.my_sub.my_third")
  end

  test "can tokenize a data path with freeform keys" do
    assert [{:name, meta, "1"}] = to_tokens("'1'")
    assert meta[:quoted] == true

    assert [{:name, _, "1"}, :., {:name, _, "2"}] = to_tokens("'1'.'2'")
  end

  test "can tokenize a data path with mixed keys" do
    assert [{:name, _, "a"}, :., {:name, _, "1"}] = to_tokens("a.'1'")

    assert [
             {:name, _, "a"},
             :.,
             {:name, _, "b"},
             :.,
             {:name, _, "1"},
             :.,
             {:name, _, "c"}
           ] = to_tokens("a.b.'1'.c")
  end

  test "can tokenize a function call" do
    assert [{:name, _, "call"}, :open_paren, {:name, _, "some"}, :close_paren] =
             to_tokens("call(some)")

    assert [
             {:name, _, "add"},
             :open_paren,
             {:literal, _, 1},
             :comma,
             {:literal, _, 2},
             :comma,
             {:literal, _, 3},
             :close_paren
           ] = to_tokens("add(1, 2, 3)")

    assert [
             {:name, _, "add"},
             :open_paren,
             {:name, _, "add"},
             :open_paren,
             {:literal, _, 1},
             :comma,
             {:literal, _, 2},
             :close_paren,
             :comma,
             {:literal, _, 3},
             :close_paren
           ] = to_tokens("add(add(1, 2), 3)")
  end

  test "can tokenize a method call" do
    assert [
             {:literal, _, 1},
             :colon,
             {:name, _, "add"},
             :open_paren,
             {:literal, _, 2},
             :close_paren
           ] = to_tokens("1:add(2)")

    assert [
             {:literal, _, 1},
             :colon,
             {:name, _, "add"},
             :open_paren,
             {:literal, _, 2},
             :close_paren,
             :colon,
             {:name, _, "add"},
             :open_paren,
             {:literal, _, 3},
             :close_paren
           ] = to_tokens("1:add(2):add(3)")

    assert [
             {:literal, _, 1},
             :colon,
             {:name, _, "add"},
             :open_paren,
             {:literal, _, 2},
             :close_paren,
             :colon,
             {:name, _, "add"},
             :open_paren,
             {:name, _, "my_var"},
             :close_paren
           ] = to_tokens("1:add(2):add(my_var)")

    assert [
             {:literal, _, 1},
             :colon,
             {:name, _, "add"},
             :open_paren,
             {:literal, _, 2},
             :close_paren,
             :colon,
             {:name, _, "add"},
             :open_paren,
             {:name, _, "add"},
             :open_paren,
             {:literal, _, 3},
             :comma,
             {:literal, _, 4},
             :close_paren,
             :close_paren
           ] = to_tokens("1:add(2):add(add(3,4))")

    assert [
             {:name, _, "some_map"},
             :colon,
             {:name, _, "size"},
             :open_paren,
             :close_paren
           ] = to_tokens("some_map:size()")
  end

  test "parse errors are returned with an exception struct" do
    assert {:error, %ParseError{}} = Expresso.tokenize("!%//()z^q")
  end

  test "can tokenize a quoted string" do
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

  test "can tokenize a lambda expression" do
    assert [
             {:name, _, "some_list"},
             :colon,
             {:name, _, "map"},
             :open_paren,
             :fn,
             :open_paren,
             {:name, _, "x"},
             :close_paren,
             :arrow,
             {:name, _, "x"},
             :end,
             :close_paren
           ] = to_tokens("some_list:map(fn(x) => x end)")
  end
end
