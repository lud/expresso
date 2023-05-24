defmodule Expresso.TokenizerTest do
  alias Expresso.Parser
  use ExUnit.Case, async: true

  defp get_tokens(code) do
    IO.puts([IO.ANSI.yellow(), code, IO.ANSI.reset()])
    assert {:ok, value_eps, buffer} = code |> Expresso.parse()

    if not Parser.empty_buffer?(buffer) do
      IO.puts([IO.ANSI.yellow(), "buffer not empty: ", IO.ANSI.reset(), elem(buffer, 1)])
    end

    IO.puts([IO.ANSI.blue(), "=> ", inspect(value_eps), IO.ANSI.reset()])
    value_eps
  end

  test "can evaluate an integer" do
    assert 1 == get_tokens("1")
    assert -1 == get_tokens("-1")
    assert 123 == get_tokens("123")
    assert -123 == get_tokens("-123")
    assert -0 == get_tokens("-0")
  end

  test "can evaluate a float" do
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

  test "can evaluate a data path" do
    assert {:dpath, _, ["a"]} = get_tokens("a")
    assert {:dpath, _, ["my_var"]} = get_tokens("my_var")
    assert {:dpath, _, ["my_var", "my_sub"]} = get_tokens("my_var.my_sub")
    assert {:dpath, _, ["my_var", "my_sub", "my_third"]} = get_tokens("my_var.my_sub.my_third")
  end

  test "can evaluate a data path with freeform keys" do
    assert {:dpath, _, ["1"]} = get_tokens("'1'")
    assert {:dpath, _, ["1", "2"]} = get_tokens("'1'.'2'")
  end

  test "can evaluate a data path with mixed keys" do
    assert {:dpath, _, ["a", "1"]} = get_tokens("a.'1'")
    assert {:dpath, _, ["a", "b", "1", "c"]} = get_tokens("a.b.'1'.c")
  end

  test "can evaluate a function call" do
    assert {:fun_call, nil, ["call", [{:dpath, nil, ["some"]}]]} = get_tokens("call(some)")
    assert {:fun_call, nil, ["add", [1, 2, 3]]} = get_tokens("add(1, 2, 3)")

    assert {:fun_call, nil, ["add", [{:fun_call, nil, ["add", [1, 2]]}, 3]]} =
             get_tokens("add(add(1, 2), 3)")
  end

  test "can evaluate a method call" do
    # assert {:fun_call, nil, ["add", [1, 2]]} = get_tokens("1:add(2)")

    assert {:fun_call, nil, ["add", [{:fun_call, nil, ["add", [1, 2]]}, 3]]} =
             get_tokens("1:add(2):add(3)")
  end
end
