defmodule Expresso.VM.Library do
  @arg_types ~w(
    string
    as_string
    object
    list
    number
    lambda
    any
  )a

  defmacro __using__(_) do
    quote location: :keep do
      import unquote(__MODULE__)
      @before_compile unquote(__MODULE__)
      Module.register_attribute(__MODULE__, :vm_function, accumulate: true)

      # Injecting all functions types to speed up things

      @doc false
      def __vm_type__(:as_string, arg) when is_binary(arg), do: {:ok, arg}
      def __vm_type__(:as_string, arg) when is_integer(arg), do: {:ok, Integer.to_string(arg)}
      def __vm_type__(:as_string, arg) when is_float(arg), do: {:ok, Float.to_string(arg)}

      def __vm_type__(:as_string, arg),
        do: {:error, "cannot use " <> typeof(arg) <> " as a string"}

      def __vm_type__(:object, arg) when is_map(arg), do: {:ok, arg}
      def __vm_type__(:object, arg), do: {:error, "not an object"}

      def __vm_type__(:string, arg) when is_binary(arg), do: {:ok, arg}
      def __vm_type__(:string, arg), do: {:error, "not a string"}

      def __vm_type__(:list, arg) when is_list(arg), do: {:ok, arg}
      def __vm_type__(:list, arg), do: {:error, "not a list"}

      def __vm_type__(:number, arg) when is_integer(arg) when is_float(arg), do: {:ok, arg}
      def __vm_type__(:number, arg), do: {:error, "not a number"}

      def __vm_type__(:lambda, {:lambda, f}) when is_function(f, 2), do: {:ok, f}
      def __vm_type__(:lambda, _), do: {:error, "not a function"}

      def __vm_type__(:any, {:lambda, f}) when is_function(f, 2), do: {:ok, f}
      def __vm_type__(:any, arg), do: {:ok, arg}

      def __vm_type__({:union_type, subtypes}, arg) do
        __vm_union_type__(arg, subtypes)
      end

      def __vm_type__({:array_type, subtype}, arg) do
        __vm_array_type__(arg, subtype)
      end

      defp __vm_union_type__(arg, subtypes), do: __vm_union_type__(arg, subtypes, [])

      defp __vm_union_type__(arg, [s | subtypes], reasons) do
        case __vm_type__(s, arg) do
          {:ok, value} -> {:ok, value}
          {:error, reason} -> __vm_union_type__(arg, subtypes, [reason | reasons])
        end
      end

      defp __vm_union_type__(arg, [], reasons) do
        {:error, Enum.join(reasons, ", ")}
      end

      defp __vm_array_type__(arg, subtype) when is_list(arg) do
        Enum.reduce_while(arg, [], fn
          elem, acc ->
            case __vm_type__(subtype, elem) do
              {:ok, value} -> {:cont, [value | acc]}
              {:error, reason} -> {:halt, {:error, reason}}
            end
        end)
        |> case do
          {:error, _} = err -> err
          list -> {:ok, :lists.reverse(list)}
        end
      end

      defp __vm_array_type__(arg, subtype) when is_list(arg) do
        Enum.reduce_while(arg, [], fn
          elem, acc ->
            case __vm_type__(subtype, elem) do
              {:ok, value} -> {:cont, [value | acc]}
              {:error, reason} -> {:halt, {:error, reason}}
            end
        end)
        |> case do
          {:error, _} = err -> err
          list -> {:ok, :lists.reverse(list)}
        end
      end

      defp __vm_array_type__(arg, subtype) do
        {:error, "not an array"}
      end

      defp typeof(list) when is_list(list), do: "array"
      defp typeof(map) when is_map(map), do: "object"
      defp typeof(binary) when is_binary(binary), do: "binary"
      defp typeof(integer) when is_integer(integer), do: "integer"
      defp typeof(float) when is_float(float), do: "float"
      # defp typeof(nil), do: "null"

      defp pull_arg(args, state, {:spread_type, t}, arg_num) do
        {values, [], state} = __pull_spread__(args, state, t, arg_num, [])
        {values, [], state}
      end

      defp pull_arg([arg | args], state, {:optional_type, {t, _}}, arg_num) do
        pull_arg([arg | args], state, t, arg_num)
      end

      defp pull_arg([], state, {:optional_type, {_, default}}, arg_num) do
        {default, [], state}
      end

      defp pull_arg([arg | args], state, type, arg_num) do
        {arg_val, state} = eval(arg, state)

        case __vm_type__(type, arg_val) do
          {:ok, value} ->
            {value, args, state}

          {:error, errmsg} ->
            {_, lc, _} = arg

            throw({:arg_type_error, arg_val, lc, arg_num, errmsg})
        end
      end

      defp pull_arg([], state, _, arg_num) do
        throw({:argument_count_error, arg_num})
      end

      # TODO optional arguments

      defp pull_args(args, state, types) do
        types
        |> Enum.with_index(1)
        |> Enum.reduce({[], args, state}, fn {t, index}, {values, args, state} ->
          {value, args, state} = pull_arg(args, state, t, index)

          {[value | values], args, state}
        end)
        |> then(fn {values, args, state} -> {:lists.reverse(values), args, state} end)
      end

      defp __pull_spread__([_ | _] = args, state, t, arg_num, acc) do
        {value, args, state} = pull_arg(args, state, t, arg_num)
        __pull_spread__(args, state, t, arg_num + 1, [value | acc])
      end

      defp __pull_spread__([] = args, state, t, arg_num, acc) do
        {:lists.reverse(acc), [], state}
      end
    end
  end

  defmacro function(signature, do: body) do
    {call, guards} =
      case signature do
        {:when, _, [call | guards]} -> {call, guards}
        _ -> {signature, []}
      end

    build_function(__CALLER__, call, guards, body)
  end

  @call_state :function_call_state
  @call_args :arguments

  defp build_function(env, _call, [_ | _] = guards, _body) do
    reraise "VM functions do not support guards, got: when #{Macro.to_string(quote do
              (unquote_splicing(guards))
            end)}",
            Macro.Env.stacktrace(env)
  end

  defp build_function(env, call, [], body) do
    mod = env.module
    {name, _, typed_args} = call
    name = Atom.to_string(name)
    # mod = env.module
    the_state = Macro.var(@call_state, mod)
    the_args = Macro.var(@call_args, mod)

    {match_vars_list, types_list} =
      typed_args
      |> Enum.map(fn
        {:"::", _, [var, type]} ->
          vm_type = build_vm_type(type)
          {var, vm_type}

        other ->
          reraise ArgumentError.exception(
                    "Invalid argument declaration: #{Macro.to_string(other)}"
                  ),
                  Macro.Env.stacktrace(env)
      end)
      |> Enum.unzip()

    pulling =
      quote do
        {unquote(match_vars_list), _, unquote(the_state)} =
          pull_args(unquote(the_args), unquote(the_state), unquote(types_list))
      end

    quote do
      @vm_function {unquote(name), __MODULE__, unquote(Macro.escape(types_list))}
      defp __vm_function__(unquote(name), unquote(the_args), unquote(the_state)) do
        unquote(pulling)
        return = unquote(body)
        {return, unquote(the_state)}
      end
    end
    |> tap(&IO.puts(["\n", Macro.to_string(&1)]))
  end

  defp build_vm_type({:|, _, types}) do
    {:union_type, Enum.map(types, &build_vm_type/1)}
  end

  defp build_vm_type({t, _, nil}) do
    ensure_type_exists(t)
    t
  end

  defp build_vm_type({:array, _, [t]}) do
    {:array_type, build_vm_type(t)}
  end

  defp build_vm_type({:=, _, [t, default_value]}) do
    default = build_default_value(default_value)
    t = build_vm_type(t)
    {:optional_type, {t, default}}
  end

  defp build_vm_type({:..., _, [t]}) do
    {:spread_type, build_vm_type(t)}
  end

  defp build_vm_type({t, _, _} = type) do
    raise ArgumentError, "invalid VM type: #{Macro.to_string(type)}"
    ensure_type_exists(t)
    t
  end

  defp build_default_value(default)
       when is_binary(default)
       when is_integer(default)
       when is_float(default) do
    default
  end

  defp build_default_value(quoted) do
    raise ArgumentError, "default value must be a litteral value, got #{inspect(quoted)}"
  end

  defp ensure_type_exists(key) when key in @arg_types, do: :ok

  defp ensure_type_exists(key) do
    raise ArgumentError, "unknown VM type #{inspect(key)}"
  end

  defmacro __before_compile__(_) do
    quote do
      defp __vm_function__(name, _, state) do
        throw(:undefined_function_error)
      end

      def __vm_library__(:functions) do
        @vm_function
      end
    end
  end
end
