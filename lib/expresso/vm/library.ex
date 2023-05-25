defmodule Expresso.VM.Library do
  alias Expresso.EvalError
  @arg_types ~w(
    string
    loose_string
    object
    list
    number
    lambda
  )a

  defmacro __using__(_) do
    quote location: :keep do
      import unquote(__MODULE__)
      @before_compile unquote(__MODULE__)
      # Injecting all functions types to speed up things
      defp __vm_type__loose_string(arg) when is_binary(arg), do: {:ok, arg}
      defp __vm_type__loose_string(arg) when is_integer(arg), do: {:ok, Integer.to_string(arg)}
      defp __vm_type__loose_string(arg) when is_float(arg), do: {:ok, Float.to_string(arg)}
      defp __vm_type__loose_string(arg), do: {:error, "cannot use #{typeof(arg)} as a string"}

      defp __vm_type__object(arg) when is_map(arg), do: {:ok, arg}
      defp __vm_type__object(arg), do: {:error, "not an object"}

      defp __vm_type__string(arg) when is_binary(arg), do: {:ok, arg}
      defp __vm_type__string(arg), do: {:error, "not a string"}

      defp __vm_type__list(arg) when is_list(arg), do: {:ok, arg}
      defp __vm_type__list(arg), do: {:error, "not a list"}

      defp __vm_type__number(arg) when is_integer(arg) when is_float(arg), do: {:ok, arg}
      defp __vm_type__number(arg), do: {:error, "not a number"}

      defp __vm_type__lambda({:lambda, f}) when is_function(f, 2), do: {:ok, f}
      defp __vm_type__lambda(_), do: {:error, "not a function"}

      defp __vm_type__any({:lambda, f}) when is_function(f, 2), do: {:ok, f}
      defp __vm_type__any(arg), do: {:ok, arg}

      defp __vm_union_type__(arg, casters), do: __vm_union_type__(arg, casters, [])

      defp __vm_union_type__(arg, [c | casters], reasons) do
        case c.(arg) do
          {:ok, value} -> {:ok, value}
          {:error, reason} -> __vm_union_type__(arg, casters, [reason | reasons])
        end
      end

      defp __vm_union_type__(arg, [], reasons) do
        {:error, Enum.join(reasons, ", ")}
      end

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

            throw({:arg_type_error, arg, lc, arg_num, errmsg})
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

      defp fetch_lc({_, lc, _}), do: lc
      defp fetch_lc(_), do: nil
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

    {unpack_vars, pack_types} =
      typed_args
      |> Enum.map(fn
        {:"::", _, [var, type]} ->
          caster = build_caster(type)
          {var, caster}

        other ->
          reraise ArgumentError.exception(
                    "Invalid argument declaration: #{Macro.to_string(other)}"
                  ),
                  Macro.Env.stacktrace(env)
      end)
      |> Enum.unzip()

    pulling =
      quote do
        {unquote(unpack_vars), _, unquote(the_state)} =
          pull_args(unquote(the_args), unquote(the_state), unquote(pack_types))
      end

    quote do
      defp __vm_function__(unquote(name), unquote(the_args), unquote(the_state)) do
        unquote(pulling)
        return = unquote(body)
        {return, unquote(the_state)}
      end
    end

    # |> tap(&IO.puts(Macro.to_string(&1)))
  end

  defp build_caster({:|, _, types}) do
    subcasters = Enum.map(types, &build_caster/1)

    quote do
      fn the_val -> __vm_union_type__(the_val, unquote(subcasters)) end
    end
  end

  defp build_caster({type_fun, _, _}) do
    ensure_type_fun(type_fun)
    vm_type_fun = :"__vm_type__#{type_fun}"
    {:&, [], [{:/, [context: Elixir, imports: []], [{vm_type_fun, [], Elixir}, 1]}]}
  end

  defp ensure_type_fun(key) when key in @arg_types, do: :ok

  defp ensure_type_fun(key) do
    raise ArgumentError, "unknown VM function type #{inspect(key)}"
  end

  defmacro __before_compile__(_) do
    quote do
      defp __vm_function__(name, _, state) do
        throw(:undefined_function_error)
      end
    end
  end
end
