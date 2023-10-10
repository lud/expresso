defmodule Expresso.VMTest do
  alias Expresso.EvalError
  alias Expresso.VM
  use ExUnit.Case, async: true

  defp run(code, data \\ %{}) do
    {:ok, tokens} = Expresso.Tokenizer.tokenize(code)
    {:ok, ast} = Expresso.Parser.parse_tokens(tokens)

    case VM.run(ast, data, debug: true) do
      {:ok, value, _state} -> value
      {:error, e} -> raise EvalError.with_source(e, code)
    end
  end

  defp get_error(code, input \\ %{}) do
    {:ok, tokens} = Expresso.Tokenizer.tokenize(code)
    {:ok, ast} = Expresso.Parser.parse_tokens(tokens)
    assert {:error, reason} = VM.run(ast, input)
    assert is_struct(reason, EvalError)
    reason = EvalError.with_source(reason, code)
    message = Exception.message(reason)

    # IO.puts([IO.ANSI.magenta(), message, IO.ANSI.reset()])

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

    # chaining error position
    get_error(~S/a_map:replace("l", "x")/, %{"a_map" => %{}})
    get_error(~S/"hello":replace("o", ""):size()/, %{"a_map" => %{}})
    get_error(~S/"hello":replace("l", a_map)/, %{"a_map" => %{}})
  end

  test "lengths and sizes" do
    values = %{
      "some_string" => "abc",
      "some_list" => ~c"abc",
      "some_map" => %{a: 1, b: 2, c: 3}
    }

    assert 3 = run(~s[some_string:len()], values)
    assert 3 = run(~s[some_list:len()], values)
    assert 3 = run(~s[some_map:size()], values)

    get_error(~s[some_map:len()], values)
  end

  test "math functions" do
    assert 3 = run("add(1, 2)")
    assert 3 = run("1:add(2)")
    assert 3.14 = run("3:add(0.14)")
  end

  test "missing arg error" do
    err = get_error("add(1)")
    assert Exception.message(err) =~ "missing 2nd argument for function `add`"
  end

  test "lambdas" do
    assert [2, 3, 4] =
             run("some_list:for_each(fn(x) => x:add(1) end)", %{"some_list" => [1, 2, 3]})

    assert [11, 12, 13] =
             run("some_list:for_each(fn(x) => x:add(amount) end)", %{
               "some_list" => [1, 2, 3],
               "amount" => 10
             })

    # var shadowing
    assert [[11], [12, 15], [13]] =
             run("some_list:for_each(fn(x) => x:for_each(fn(x) => x:add(amount) end) end)", %{
               "some_list" => [[1], [2, 5], [3]],
               "amount" => 10
             })

    # missing arguments
    err =
      get_error("some_list:for_each(fn(x, some_required_arg) => x:add(amount) end)", %{
        "some_list" => [1, 2, 3],
        "amount" => 10
      })

    assert Exception.message(err) =~ "missing 2nd argument `some_required_arg`"

    # extra arguments are OK
  end

  test "reduce lambda" do
    # with initializer
    assert 110 =
             run("some_list:reduce(100, fn(x, acc) => acc:add(x) end)", %{
               "some_list" => [1, 2, 3, 4]
             })

    # empty list with initializer
    assert 100 =
             run("some_list:reduce(100, fn(x, acc) => acc:add(x) end)", %{
               "some_list" => []
             })

    # without initializer
    assert 10 =
             run("some_list:reduce(fn(x, acc) => acc:add(x) end)", %{
               "some_list" => [1, 2, 3, 4]
             })

    assert 0 =
             run("some_list:reduce(fn(x, acc) => acc:add(x) end)", %{
               "some_list" => [0]
             })

    # empty list without initializer
    err =
      get_error("reduce(some_list, fn(x, acc) => acc:add(x) end)", %{
        "some_list" => []
      })

    assert Exception.message(err) =~ "empty list given to `reduce` without initial value"
  end

  test "the VM supports array types" do
    err = get_error(~s/"aaa":join("")/)
    assert Exception.message(err) =~ "not an array"

    assert "hello!world!test" =
             run(~s/some_list:join("!")/, %{"some_list" => ["hello", "world", "test"]})
  end

  test "the VM supports default arguments" do
    assert "helloworldtest" =
             run(~s/some_list:join()/, %{"some_list" => ["hello", "world", "test"]})
  end

  test "the VM supports spread arguments" do
    data = %{"a" => "hello", "b" => "world", "c" => "test"}
    assert "hello" = run(~s/concat(a)/, data)
    assert "helloworld" = run(~s/concat(a,b)/, data)
    assert "hello world!" = run(~s/concat(a, " ", b, "!")/, data)
    assert "" = run("concat()")
  end

  test "scoped lambdas" do
    data = %{"x" => 1}

    assert 1 = run("x", data)
    assert 1 = run("fn() => x end:call()", data)
    assert 1 = run("fn(y) => y end:call(x)", data)
    assert 2 = run("fn(y) => y end:call(x:add(1))", data)

    # x is shadowed and should be bound to `2`
    assert 2 = run("fn(x) => x end:call(2)", data)

    # x is shadowed and passed in a closure
    assert 2 = run("fn(x) => fn() => x end end:call(2):call()", data)
    assert 3 = run("fn(x) => fn(x) => x end end:call(2):call(3)", data)
    assert 3 = run("fn(x) => fn(x) => x end:call(x:add(1)) end:call(2)", data)

    # same variable name for both args should return the latter
    assert 2 = run("fn(x, x) => x end:call(1, 2)", data)
  end

  test "fun with lambdas" do
    # some messy stuff
    data = %{"some_list" => [1, 2, 3, 4]}

    # - In the first for_each, we're returning a lambda that returns `x`, where
    #   `x` is the number in the list.
    # - In the second for_each, `x` refers to the returned lambda.
    code = ~s"""
    some_list
      :for_each(fn(x) => fn(y) => x:add(y) end end)
      :for_each(fn(x) => x:call(10) end)
    """

    assert [11, 12, 13, 14] = run(code, data)
  end

  test "returning lambdas from functions" do
    data = %{"some_list" => [1, 2, 3, 4]}

    code = ~s"""
    some_list
      :for_each(fn(x) => fn(y) => x:add(y) end end)
      :for_each(fn(f) => f:call(10) end)
    """

    assert [11, 12, 13, 14] = run(code, data)
  end
end
