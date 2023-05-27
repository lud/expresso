defmodule Expresso.AutocompleteTest do
  alias Expresso.VM
  alias Expresso.Tokenizer
  alias Expresso.ParseError
  alias Expresso.Parser
  use ExUnit.Case, async: true

  defp get_completions(code, data \\ %{}) do
    assert {:ok, completions} = Expresso.get_completions(code, data)
    completions
  end

  defp find_completion(comps, predicate) do
    Enum.find(comps, predicate)
  end

  defp only_data(comps) do
    Enum.filter(comps, &match?(%{type: t} when t == :data, &1))
  end

  describe "path completions -" do
    test "returns a list" do
      assert is_list(get_completions(""))
    end

    test "no code, returns a list of root paths" do
      data = %{"aaa" => 1, "bbb" => 2, "ccc" => 3}

      comps = get_completions("", data)

      assert find_completion(comps, &match?(%{type: :data, comp: "aaa", key: "aaa"}, &1))
      assert find_completion(comps, &match?(%{type: :data, comp: "bbb", key: "bbb"}, &1))
      assert find_completion(comps, &match?(%{type: :data, comp: "ccc", key: "ccc"}, &1))
    end

    test "returns keys matching a prefix" do
      data = %{"aaa" => 1, "bbb" => 2, "ccc" => 3}

      comps = get_completions("a", data)

      assert find_completion(comps, &match?(%{type: :data, comp: "aa", key: "aaa"}, &1))
      refute find_completion(comps, &match?(%{type: :data, key: "bbb"}, &1))
      refute find_completion(comps, &match?(%{type: :data, key: "ccc"}, &1))
    end

    test "returns the child keys of a matched map with a dot prefix" do
      data = %{"parent" => %{"aaa" => 1, "bbb" => 2}}

      # we do not provide the dot here
      comps = get_completions("parent", data)

      assert find_completion(comps, &match?(%{type: :data, comp: ".aaa", key: "aaa"}, &1))
      assert find_completion(comps, &match?(%{type: :data, comp: ".bbb", key: "bbb"}, &1))
    end

    test "returns the child keys of a matched map after the dot" do
      data = %{"parent" => %{"aaa" => 1, "bbb" => 2}}

      # we DO provide the dot here
      comps = get_completions("parent.", data)

      assert find_completion(comps, &match?(%{type: :data, comp: "aaa", key: "aaa"}, &1))
      assert find_completion(comps, &match?(%{type: :data, comp: "bbb", key: "bbb"}, &1))
    end

    test "returns the child keys matching a prefix after the dot" do
      data = %{"parent" => %{"aaa" => 1, "bbb" => 2}}

      # we DO provide the dot here
      [comp] = get_completions("parent.a", data) |> only_data()
      assert match?(%{type: :data, comp: "aa", key: "aaa"}, comp)
    end

    test "does not return anything if the path is wrong" do
      data = %{"parent" => %{"aaa" => 1, "bbb" => 2}}

      assert [] == get_completions("parent.c", data) |> only_data()
      assert [] == get_completions("par.", data) |> only_data()
      assert [] == get_completions("par.aaa", data) |> only_data()
      assert [] == get_completions("parentaaa", data) |> only_data()
    end

    test "does not return anything if the path is not a map" do
      data = %{"name" => "Joe"}

      assert [] == get_completions("name", data) |> only_data()
    end

    test "does not return anything if there is no path" do
      data = %{"a" => 1, "b" => 2, "c" => 3}
      assert [] == get_completions("replace(a, b, c)") |> only_data()
    end

    test "completes the path within other expressions" do
      data = %{"parent" => %{"aaa" => 1, "bbb" => 2}}

      # we DO provide the dot here
      comps = get_completions("replace(parent.", data)

      assert find_completion(comps, &match?(%{type: :data, comp: "aaa", key: "aaa"}, &1))
      assert find_completion(comps, &match?(%{type: :data, comp: "bbb", key: "bbb"}, &1))

      assert comps == get_completions("stuff:some_method(parent.", data)
      assert comps == get_completions("stuff:some_method(fn(x) => parent.", data)
      assert comps == get_completions("stuff:some_method(fn(x) => parent.", data)
    end
  end

  describe "function completions -" do
    test "a VM library can list its functions" do
      Code.ensure_loaded!(VM)
      assert is_list(VM.library_info(VM, :functions))

      assert {"replace", VM, [:as_string, :as_string, :as_string]} in VM.library_info(
               VM,
               :functions
             )
    end

    test "completes all functions with no code" do
      data = %{"aaa" => 1}

      comps = get_completions("", data)

      # we still have the data
      assert find_completion(comps, &match?(%{type: :data, comp: "aaa", key: "aaa"}, &1))

      # but also the functions
      assert find_completion(comps, &match?(%{type: :fun, comp: "replace(", fun: "replace"}, &1))
    end

    test "completes with prefix" do
      comps = get_completions("rep")
      assert find_completion(comps, &match?(%{type: :fun, fun: "replace", comp: "lace("}, &1))
    end

    test "completes with prefix and data with same name" do
      data = %{"replacer" => "hello"}
      comps = get_completions("rep", data)
      assert find_completion(comps, &match?(%{type: :fun, fun: "replace", comp: "lace("}, &1))
      assert find_completion(comps, &match?(%{type: :data, key: "replacer", comp: "lacer"}, &1))

      # with exact same name
      data = %{"replace" => "hello"}
      comps = get_completions("rep", data)
      assert find_completion(comps, &match?(%{type: :fun, fun: "replace", comp: "lace("}, &1))
      assert find_completion(comps, &match?(%{type: :data, key: "replace", comp: "lace"}, &1))

      # functions are discared with props
      data = %{"parent" => %{"replace" => "hello"}}
      comps = get_completions("parent.rep", data)
      refute find_completion(comps, &match?(%{type: :fun, fun: "replace", comp: "lace("}, &1))
      assert find_completion(comps, &match?(%{type: :data, key: "replace", comp: "lace"}, &1))
    end

    test "completes with methods from the data" do
      # with data as a string

      data = %{"greeting" => "hello"}
      comps = get_completions("greeting", data)

      assert find_completion(
               comps,
               &match?(%{type: :fun, fun: "replace", comp: ":replace("}, &1)
             )

      assert find_completion(comps, &match?(%{type: :fun, fun: "len", comp: ":len("}, &1))
      refute find_completion(comps, &match?(%{type: :fun, fun: "size"}, &1))

      # with data as a map

      data = %{"some_map" => %{}}
      comps = get_completions("some_map", data)
      assert find_completion(comps, &match?(%{type: :fun, fun: "size", comp: ":size("}, &1))
      refute find_completion(comps, &match?(%{type: :fun, fun: "len"}, &1))
    end

    test "completes method names" do
      data = %{"greeting" => "hello"}
      comps = get_completions("greeting:", data)

      assert find_completion(
               comps,
               &match?(%{type: :fun, fun: "replace", comp: "replace("}, &1)
             )
    end

    test "completes method names with prefix" do
      # with data as a string we get completion for a string method
      data = %{"greeting" => "hello"}
      comps = get_completions("greeting:rep", data)

      assert find_completion(
               comps,
               &match?(%{type: :fun, fun: "replace", comp: "lace("}, &1)
             )

      # but with some map we get nothing with the "rep" prefix
      data = %{"some_map" => %{}}
      comps = get_completions("some_map:rep", data)

      refute find_completion(
               comps,
               &match?(%{type: :fun, fun: "replace", comp: "lace("}, &1)
             )
    end

    test "autocomplete special cases" do
      data = %{"a" => %{}, "b" => 2, "c" => 3, "d" => 4}

      # nothing should come up
      assert [] = get_completions("a:b:c:d", data)

      # no completions for data should show after a colon
      assert [] = get_completions("a:", data) |> only_data()
    end
  end
end
