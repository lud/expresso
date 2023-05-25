defmodule Expresso.VM.Library do
  defmacro __using__(_) do
    quote do
      import unquote(__MODULE__)

      # Injecting all functions types to speed up things
      defp loose_string(arg) when is_binary(arg), do: {:ok, arg}
      defp loose_string(arg) when is_integer(arg), do: {:ok, Integer.to_string(arg)}
      defp loose_string(arg) when is_float(arg), do: {:ok, Float.to_string(arg)}
      defp loose_string(arg), do: {:error, "cannot use #{typeof(arg)} as a string"}

      defp typeof(list) when is_list(list), do: "array"
      defp typeof(map) when is_map(map), do: "object"
      defp typeof(binary) when is_binary(binary), do: "binary"
      defp typeof(integer) when is_integer(integer), do: "integer"
      defp typeof(float) when is_float(float), do: "float"

      defp pull_arg([arg | args], state, caster, arg_num) do
        {arg_val, state} = eval(arg, state)

        case caster.(arg_val) do
          {:ok, value} ->
            {value, args, state}

          {:error, errmsg} ->
            lc = fetch_lc(arg)

            throw({:arg_error, arg, lc, arg_num, errmsg})
        end
      end

      defp fetch_lc({_, lc, _}), do: lc
      defp fetch_lc(_), do: nil
    end
  end

  defmacro function(call, do: body) do
    build_function(__CALLER__.module, call, body)
  end

  # defmacro function(name, do: body) do
  #   build_function(__CALLER__, name, [], body)
  # end

  # defmacro function(name, arg1, arg2, do: body) do
  #   build_function(__CALLER__, name, [arg1, arg2], body)
  # end

  # defmacro function(name, arg1, arg2, arg3, do: body) do
  #   build_function(__CALLER__, name, [arg1, arg2, arg3], body)
  # end

  @call_state :function_call_state
  @call_args :arguments

  defp build_function(mod, call, body) do
    {name, _, typed_args} = call
    name = Atom.to_string(name)
    # mod = env.module
    the_state = Macro.var(@call_state, mod)
    the_args = Macro.var(@call_args, mod)

    pulling =
      typed_args
      |> Enum.map(fn {:"::", _, [var, type]} ->
        {type_fun, _, _} = type
        ensure_type_fun(type_fun)
        fn1 = {:&, [], [{:/, [context: Elixir, imports: []], [{type_fun, [], Elixir}, 1]}]}
        {var, fn1}
      end)
      |> Enum.with_index(1)
      |> Enum.map(fn {{{var, _meta, nil}, caster_call}, index} ->
        the_var = Macro.var(var, nil)

        quote do
          {unquote(the_var), unquote(the_args), unquote(the_state)} =
            pull_arg(unquote(the_args), unquote(the_state), unquote(caster_call), unquote(index))
        end
      end)
      |> then(&{:__block__, [], &1})

    quote do
      def __fun_impl__(unquote(name), unquote(the_args), unquote(the_state)) do
        unquote(pulling)
        {unquote(body), unquote(the_state)}
      end
    end
    |> tap(&IO.puts("function: " <> Macro.to_string(&1)))
  end

  @arg_types ~w(
    loose_string
  )a

  defp ensure_type_fun(key) when key in @arg_types, do: :ok

  defp ensure_type_fun(key) do
    raise ArgumentError, "unknown VM function type #{inspect(key)}"
  end
end
