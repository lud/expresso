defmodule Expresso.AutocompleteTest do
  alias Expresso.VM
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

      assert find_completion(comps, &match?(%{type: :data, comp: "aaa", label: "aaa"}, &1))
      assert find_completion(comps, &match?(%{type: :data, comp: "bbb", label: "bbb"}, &1))
      assert find_completion(comps, &match?(%{type: :data, comp: "ccc", label: "ccc"}, &1))
    end

    test "returns keys matching a prefix" do
      data = %{"aaa" => 1, "bbb" => 2, "ccc" => 3}

      comps = get_completions("a", data)

      assert find_completion(comps, &match?(%{type: :data, comp: "aa", label: "aaa"}, &1))
      refute find_completion(comps, &match?(%{type: :data, label: "bbb"}, &1))
      refute find_completion(comps, &match?(%{type: :data, label: "ccc"}, &1))
    end

    test "returns the child keys of a matched map with a dot prefix" do
      data = %{"parent" => %{"aaa" => 1, "bbb" => 2}}

      # we do not provide the dot here
      comps = get_completions("parent", data)

      assert find_completion(comps, &match?(%{type: :data, comp: ".aaa", label: "aaa"}, &1))
      assert find_completion(comps, &match?(%{type: :data, comp: ".bbb", label: "bbb"}, &1))
    end

    test "returns the child keys of a matched map after the dot" do
      data = %{"parent" => %{"aaa" => 1, "bbb" => 2}}

      # we DO provide the dot here
      comps = get_completions("parent.", data)

      assert find_completion(comps, &match?(%{type: :data, comp: "aaa", label: "aaa"}, &1))
      assert find_completion(comps, &match?(%{type: :data, comp: "bbb", label: "bbb"}, &1))
    end

    test "returns the child keys matching a prefix after the dot" do
      data = %{"parent" => %{"aaa" => 1, "bbb" => 2}}

      # we DO provide the dot here
      [comp] = get_completions("parent.a", data) |> only_data()
      assert match?(%{type: :data, comp: "aa", label: "aaa"}, comp)
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

    test "completes the path within other expressions" do
      data = %{"parent" => %{"aaa" => 1, "bbb" => 2}}

      # we DO provide the dot here
      comps = get_completions("replace(parent.", data)

      assert find_completion(comps, &match?(%{type: :data, comp: "aaa", label: "aaa"}, &1))
      assert find_completion(comps, &match?(%{type: :data, comp: "bbb", label: "bbb"}, &1))

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
      assert find_completion(comps, &match?(%{type: :data, comp: "aaa", label: "aaa"}, &1))

      # but also the functions
      assert find_completion(
               comps,
               &match?(%{type: :fun, comp: "replace(", label: "replace"}, &1)
             )
    end

    test "completes with prefix" do
      comps = get_completions("rep")
      assert find_completion(comps, &match?(%{type: :fun, label: "replace", comp: "lace("}, &1))
    end

    test "completes with prefix and data with same name" do
      data = %{"replacer" => "hello"}
      comps = get_completions("rep", data)
      assert find_completion(comps, &match?(%{type: :fun, label: "replace", comp: "lace("}, &1))
      assert find_completion(comps, &match?(%{type: :data, label: "replacer", comp: "lacer"}, &1))

      # with exact same name
      data = %{"replace" => "hello"}
      comps = get_completions("rep", data)
      assert find_completion(comps, &match?(%{type: :fun, label: "replace", comp: "lace("}, &1))
      assert find_completion(comps, &match?(%{type: :data, label: "replace", comp: "lace"}, &1))

      # functions can follow props - does not mistake the prop for a function
      data = %{"parent" => %{"replace" => "hello"}}
      comps = get_completions("parent.rep", data)
      refute find_completion(comps, &match?(%{type: :fun, label: "replace"}, &1))
      assert find_completion(comps, &match?(%{type: :data, label: "replace", comp: "lace"}, &1))

      # functions can follow props - uses the data
      data = %{"parent" => %{"greeting" => "hello"}}

      # without colon
      comps = get_completions("parent.greeting", data)

      assert find_completion(
               comps,
               &match?(%{type: :fun, label: "replace", comp: ":replace("}, &1)
             )

      refute find_completion(comps, &match?(%{type: :fun, label: "size"}, &1))

      # with colon
      comps = get_completions("parent.greeting:", data)

      assert find_completion(
               comps,
               &match?(%{type: :fun, label: "replace", comp: "replace("}, &1)
             )

      refute find_completion(comps, &match?(%{type: :fun, label: "size"}, &1))
    end

    test "completes with methods from the data" do
      # with data as a string

      data = %{"greeting" => "hello"}
      comps = get_completions("greeting", data)

      assert find_completion(
               comps,
               &match?(%{type: :fun, label: "replace", comp: ":replace("}, &1)
             )

      assert find_completion(comps, &match?(%{type: :fun, label: "len", comp: ":len("}, &1))
      refute find_completion(comps, &match?(%{type: :fun, label: "size"}, &1))

      # with data as a map

      data = %{"some_map" => %{}}
      comps = get_completions("some_map", data)
      assert find_completion(comps, &match?(%{type: :fun, label: "size", comp: ":size("}, &1))
      refute find_completion(comps, &match?(%{type: :fun, label: "len"}, &1))
    end

    test "completes with all known functions when the data is now known" do
      # In the following code we have a semicolon, and as the autocompletion tool does
      # not run the code, it cannot the type of the method subject, so it should
      # return all known functions.
      code = ~s/some_map.a_key:replace("a", "b"):/
      data = %{}
      comps = get_completions(code, data)
      # This always works as long as the data is a map
      assert find_completion(comps, &match?(%{type: :fun, label: "size", comp: "size("}, &1))
      # But this should work too
      assert find_completion(
               comps,
               &match?(%{type: :fun, label: "replace", comp: "replace("}, &1)
             )
    end

    test "completes method names" do
      data = %{"greeting" => "hello"}
      comps = get_completions("greeting:", data)

      assert find_completion(
               comps,
               &match?(%{type: :fun, label: "replace", comp: "replace("}, &1)
             )
    end

    test "completes method names with prefix" do
      # with data as a string we get completion for a string method
      data = %{"greeting" => "hello"}
      comps = get_completions("greeting:rep", data)

      assert find_completion(
               comps,
               &match?(%{type: :fun, label: "replace", comp: "lace("}, &1)
             )

      # but with some map we get nothing with the "rep" prefix
      data = %{"some_map" => %{}}
      comps = get_completions("some_map:rep", data)

      refute find_completion(
               comps,
               &match?(%{type: :fun, label: "replace", comp: "lace("}, &1)
             )
    end

    test "autocomplete special cases" do
      data = %{"a" => %{}, "b" => 2, "c" => 3, "x" => 4}

      # nothing should come up
      assert [] = get_completions("a:b:c:x", data)

      # no completions for data should show after a colon
      assert [] = get_completions("a:", data) |> only_data()
    end
  end
end
