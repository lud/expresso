defmodule Expresso.Completions do
  alias Expresso.VM

  if Mix.env() == :prod do
    def from_tokens(tokens, data) do
      _from_tokens(tokens, data)
    rescue
      _ -> []
    end
  else
    def from_tokens(tokens, data) do
      _from_tokens(tokens, data)
    end
  end

  defp _from_tokens(tokens, data) do
    tokens = :lists.reverse(tokens)
    data_completions = build_data_completions(tokens, data)
    fun_completions = build_fun_completions(tokens, data)
    data_completions ++ fun_completions
  end

  # -- Data structure completions ---------------------------------------------

  defp build_data_completions(tokens, data) do
    tokens
    |> rewind_data_path()
    |> complete_data(data)
  end

  defp rewind_data_path(tokens) do
    Enum.reduce_while(tokens, [], fn
      {:name, _, _} = token, acc -> {:cont, [token | acc]}
      :. = token, acc -> {:cont, [token | acc]}
      :colon = token, acc -> {:cont, [token | acc]}
      _, acc -> {:halt, acc}
    end)
  end

  defp complete_data(tokens, data) do
    tokens
    # Init acc with the current data and a list of possible completions.  If the
    # reducer returns nil as the data instead of halting, meaning it did not
    # match any data, this allow the next iteration to run, if there is any, and
    # so void the whole completion and return an empty list.
    |> Enum.reduce_while({data, {:map_keys, data}}, fn token, {data, completions} ->
      data_reducer(token, {data, completions})
    end)

    # Now extract the proposed completions and run them
    |> elem(1)
    |> eval_completions()
  end

  # If the given key exist in the data, discard the current completions and
  # continue with the value of the key as the current data, and a list of
  # all completions from that value, with a dot prefix

  defp data_reducer({:name, _, key}, {data, _}) when is_map_key(data, key) do
    data = Map.fetch!(data, key)
    completions = if is_map(data), do: {:dot_map_keys, data}, else: :no_completion
    {:cont, {data, completions}}
  end

  # If the given key is not present in the map, we filter the keys that are
  # a binary and start with the prefix. We return no data as we could not
  # match any data, and return a completion of the keys.
  defp data_reducer({:name, _, prefix}, {data, _}) when is_map(data) do
    keys =
      data
      |> Enum.filter(fn {k, _} -> is_binary(k) and String.starts_with?(k, prefix) end)
      |> Enum.map(&elem(&1, 0))

    {:cont, {nil, {:keys, keys, prefix}}}
  end

  # When we encounter a dot, we can just continue with the current data.
  defp data_reducer(:., {data, _}) when is_map(data) do
    {:cont, {data, {:map_keys, data}}}
  end

  # If we encounter a colon we stop
  defp data_reducer(:colon, _) do
    {:halt, {nil, :no_completion}}
  end

  # No data found, halt
  defp data_reducer(_, {nil, _}) do
    {:halt, {nil, :no_completion}}
  end

  # -- Function & Method completions ------------------------------------------

  defp build_fun_completions(tokens, data) do
    tokens
    |> rewind_fun_path()
    |> complete_fun(data)
  end

  defp rewind_fun_path(tokens) do
    Enum.reduce_while(tokens, [], fn
      # not a function expression
      :., _ -> {:halt, []}
      {:name, _, _} = token, acc -> {:cont, [token | acc]}
      :colon = token, acc -> {:cont, [token | acc]}
      _, acc -> {:halt, acc}
    end)
  end

  defp complete_fun(tokens, data) do
    methods_only? = false

    tokens
    # See complete_data/2 for a basic understanding on how we build the
    # completions incrementally.
    |> Enum.reduce_while({data, :all_funs, methods_only?}, fn token, {data, completions, m?} ->
      fun_reducer(token, data, completions, m?)
    end)
    |> elem(1)
    |> eval_completions()
  end

  # if the given key exists in the data we will return a list of all applicable
  # methods. But we will also return functions that start with that name, if any
  defp fun_reducer({:name, _, fun}, data, _, m?) when is_map_key(data, fun) do
    data = Map.fetch!(data, fun)

    comps =
      if m?,
        do: {:colon_methods_for, data},
        else: [{:colon_methods_for, data}, {:fun_prefix, fun}]

    {:cont, {data, comps, m?}}
  end

  # With a name that is not a found key, if we are returning methods we will
  # return all methods with the prefix matching the data type. If we are not in
  # a methods_only situation we return all functions with the prefix.
  defp fun_reducer({:name, _, fun}, data, _, m?) do
    if is_map(data) do
      false = is_map_key(data, fun)
    end

    comps =
      if m?,
        do: {:methods_for_prefix, data, fun},
        else: {:fun_prefix, fun}

    {:cont, {nil, comps, m?}}
  end

  defp fun_reducer(:colon, data, _, _) do
    # Once we encounter a colon we will turn the methods_only flag to true
    {:cont, {data, {:methods_for, data}, true}}
  end

  # -- Format completions to be returned --------------------------------------

  defp eval_completions({:map_keys, data}) do
    data
    |> Map.keys()
    |> Enum.map(fn key -> %{type: :data, label: key, comp: key} end)
  end

  defp eval_completions({:keys, keys, prefix}) do
    keys
    |> Enum.map(fn key -> %{type: :data, label: key, comp: unprefix(key, prefix)} end)
  end

  defp eval_completions({:dot_map_keys, data}) do
    data
    |> Map.keys()
    |> Enum.map(fn key -> %{type: :data, label: key, comp: "." <> key} end)
  end

  defp eval_completions(:all_funs) do
    vm_functions()
    |> Enum.map(fn {fun, _, _} -> %{type: :fun, label: fun, comp: fun <> "("} end)
  end

  defp eval_completions({:fun_prefix, prefix}) do
    vm_functions()
    |> Enum.filter(fn {fun, _, _} -> String.starts_with?(fun, prefix) end)
    |> Enum.map(fn {fun, _, _} -> %{type: :fun, label: fun, comp: unprefix(fun, prefix) <> "("} end)
  end

  defp eval_completions({:methods_for, data}) do
    data
    |> methods_for()
    |> Enum.map(fn {fun, _, _} -> %{type: :fun, label: fun, comp: fun <> "("} end)
  end

  defp eval_completions({:colon_methods_for, data}) do
    data
    |> methods_for()
    |> Enum.map(fn {fun, _, _} -> %{type: :fun, label: fun, comp: ":" <> fun <> "("} end)
  end

  defp eval_completions({:methods_for_prefix, data, prefix}) do
    data
    |> methods_for(prefix)
    |> Enum.map(fn {fun, _, _} -> %{type: :fun, label: fun, comp: unprefix(fun, prefix) <> "("} end)
  end

  defp eval_completions(list) when is_list(list) do
    Enum.flat_map(list, &eval_completions/1)
  end

  defp eval_completions(:no_completion) do
    []
  end

  defp unprefix(string, prefix) do
    <<_::binary-size(byte_size(prefix)), rest::binary>> = string
    # String.slice(key, String.length(prefix), String.length(key))
    rest
  end

  defp vm_functions do
    # TODO allow passing options for the completions so we can list all loaded
    # modules.
    VM.library_info(VM, :functions)
  end

  defp methods_for(data, prefix \\ nil) do
    with_args =
      vm_functions()
      # Discard functions that do not take arguments
      |> Enum.filter(fn
        {_, _, [] = _no_args} -> false
        _ -> true
      end)

    with_prefix =
      case prefix do
        nil -> with_args
        _ -> Enum.filter(with_args, fn {fun, _, _} -> String.starts_with?(fun, prefix) end)
      end

    with_prefix
    # Keep functions that accept the data as the first argument
    |> Enum.filter(fn {_, mod, [t1 | _] = _no_args} -> of_type?(data, mod, t1) end)
  end

  defp of_type?(data, mod, {:spread_type, t}) do
    of_type?(data, mod, t)
  end

  defp of_type?(data, mod, t) do
    case mod.__vm_type__(t, data) do
      {:ok, _} -> true
      _ -> false
    end
  end
end
