defmodule Expresso.ParserTest do
  alias Expresso.ParseError
  alias Expresso.Parser
  use ExUnit.Case, async: true
  import Expresso.Test.Util

  test "can parse an integer" do
    assert 1 == get_tokens("1")
    assert -1 == get_tokens("-1")
    assert 123 == get_tokens("123")
    assert -123 == get_tokens("-123")
    assert -0 == get_tokens("-0")
  end

  test "can parse a float" do
    assert 1.1 == get_tokens("1.1")
    assert 1.0e10 == get_tokens("1.0e10")
    assert 1.0e+10 == get_tokens("1.0e+10")
    assert 1.0e-10 == get_tokens("1.0e-10")
    assert 1.0e10 == get_tokens("1.0E10")
    assert 1.0e+10 == get_tokens("1.0E+10")
    assert 1.0e-10 == get_tokens("1.0E-10")
    assert -1.0e10 == get_tokens("-1.0E10")
    assert -1.0e+10 == get_tokens("-1.0E+10")
    assert -1.0e-10 == get_tokens("-1.0E-10")
  end

  test "can parse a data path" do
    assert {:dpath, _, ["a"]} = get_tokens("a")
    assert {:dpath, _, ["my_var"]} = get_tokens("my_var")
    assert {:dpath, _, ["my_var", "my_sub"]} = get_tokens("my_var.my_sub")
    assert {:dpath, _, ["my_var", "my_sub", "my_third"]} = get_tokens("my_var.my_sub.my_third")
  end

  test "can parse a data path with freeform keys" do
    assert {:dpath, [line: 0, column: 0], ["1"]} = get_tokens("'1'")
    assert {:dpath, _, ["1", "2"]} = get_tokens("'1'.'2'")
  end

  test "can parse a data path with mixed keys" do
    assert {:dpath, _, ["a", "1"]} = get_tokens("a.'1'")
    assert {:dpath, _, ["a", "b", "1", "c"]} = get_tokens("a.b.'1'.c")
  end

  test "can parse a function call" do
    assert {:fun_call, nil, ["call", [{:dpath, [line: 0, column: 5], ["some"]}]]} =
             get_tokens("call(some)")

    assert {:fun_call, nil, ["add", [1, 2, 3]]} = get_tokens("add(1, 2, 3)")

    assert {:fun_call, nil, ["add", [{:fun_call, nil, ["add", [1, 2]]}, 3]]} =
             get_tokens("add(add(1, 2), 3)")
  end

  test "can parse a method call" do
    assert {:fun_call, nil, ["add", [1, 2]]} = get_tokens("1:add(2)")

    assert {:fun_call, nil, ["add", [{:fun_call, nil, ["add", [1, 2]]}, 3]]} =
             get_tokens("1:add(2):add(3)")

    assert {:fun_call, nil, ["add", [{:fun_call, nil, ["add", [1, 2]]}, {:dpath, _, ["my_var"]}]]} =
             get_tokens("1:add(2):add(my_var)")

    assert {:fun_call, nil,
            ["add", [{:fun_call, nil, ["add", [1, 2]]}, {:fun_call, nil, ["add", [3, 4]]}]]} =
             get_tokens("1:add(2):add(add(3,4))")
  end

  test "parse errors are returned" do
    assert {:error, %ParseError{} = err} = Expresso.parse("!%//()z^q")
  end
end
