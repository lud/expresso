# credo:disable-for-this-file Credo.Check.Refactor.Nesting
defmodule Expresso.Parser do
  alias Expresso.ParseError
  # Combinators adapted from https://gist.github.com/sasa1977/beaeb43d39b055ecb93b937123b633d5

  require Record

  Record.defrecordp(:context,
    tokens: [],
    marks: %{},
    consumed: 0,
    level: 0,
    trace: [],
    debug: false,
    on_failure: nil,
    on_consumed: nil,
    on_attempt: nil
  )

  def parse_tokens(tokens, opts \\ []) do
    ctx = context(tokens: tokens)
    debug? = Keyword.get(opts, :debug, false)
    ctx = if debug?, do: setup_debug(ctx), else: ctx

    result = parse_recursive(expr(), ctx)

    debug_info = clean_get_debug()

    final_ctx =
      case result do
        {:ok, _, context(tokens: []) = ctx} -> ctx
        _ -> nil
      end

    if debug?, do: format_debug(debug_info, final_ctx)

    return(result)
  after
    clean_get_debug()
  end

  defp parse_recursive(parser, ctx) do
    case parser.(ctx) do
      {:ok, expression, context(tokens: []) = ctx} ->
        {:ok, unwrap_expr(expression), ctx}

      {:ok, expression, context(tokens: rest) = ctx} ->
        parse_recursive(parser, context(ctx, tokens: [expression | rest]))

      other ->
        other
    end
  end

  defp return(result) do
    case result do
      {:ok, expression, context(tokens: [])} ->
        {:ok, expression}

      {:ok, result, context(tokens: rest)} ->
        {:error,
         ParseError.exception(
           message: """
           extra code: #{inspect(rest)}

           tokens: #{inspect(result)}
           """
         )}

      {:error, reason} when is_binary(reason) ->
        {:error, ParseError.exception(message: reason)}

      {:error, reason} ->
        {:error, ParseError.exception(message: inspect(reason))}
    end
  end

  defp setup_debug(ctx) do
    Process.put(failures_pkey(), [])

    context(ctx,
      debug: true,
      # Uses the level and tag given by the debug() parser and puts them in the current trace
      on_consumed: fn tag, data, context(trace: trace, level: level) = ctx ->
        context(ctx, trace: [{:took, level, tag, data} | trace])
      end,
      on_attempt: fn tag, context(trace: trace, level: level) = ctx ->
        context(ctx, trace: [{:attempt, level, tag} | trace])
      end,
      # Takes the current trace and puts it in the process dictionnary
      on_failure: fn
        context(consumed: c, trace: trace, level: level), reason ->
          trace_with_failure = [{:failure, level, reason} | trace]
          traces = Process.get(failures_pkey(), [])
          Process.put(failures_pkey(), [{:trace, c, trace_with_failure} | traces])
          :ok
      end
    )
  end

  defp failures_pkey, do: {__MODULE__, :failures}

  defp clean_get_debug do
    Process.delete(failures_pkey())
  end

  defp format_debug(nil, _) do
    IO.puts("debug info is nil")
  end

  defp format_debug([], _) do
    IO.puts("debug info is empty")
  end

  defp format_debug(traces, result) do
    trace =
      case result do
        context(trace: trace) ->
          trace

        _ ->
          {_, _, trace} = Enum.max_by(traces, fn {:trace, consumed, _} -> consumed end)
          trace
      end

    # The the farthest trace we have
    trace
    |> :lists.reverse()
    |> Enum.map_intersperse("\n", fn
      {:took, level, tag, data} ->
        [List.duplicate(" ", level * 2), inspect(tag), " = ", inspect(data)]

      {:attempt, level, tag} ->
        [List.duplicate(" ", level * 2), inspect(tag)]

      {:failure, level, reason} ->
        [List.duplicate(" ", level * 2), "FAIL ", inspect(reason)]
    end)
    |> IO.puts()
  end

  defp lazy(combinator) do
    fn input ->
      parser = combinator.()
      parser.(input)
    end
  end

  defp expr() do
    # debug :expr,
    choice([
      method_call_chain(),
      getprop_chain(),
      lambda_expr(),
      function_call(),
      literal(),
      name()
    ])
    |> map(fn expression -> {:expr, [], expression} end)
  end

  defp unwrap_expr({:expr, _, expression}), do: expression

  defp sub_expr do
    choice([
      token(:expr),
      lazy(fn -> expr() end)
    ])
    |> map(&unwrap_expr/1)
  end

  defp method_call_chain do
    # debug :method_call_chain,
    exclusive(
      :in_method_call_chain,
      sequence([
        sub_expr(),
        many1(method_call())
      ])
    )
    |> map(fn [subject, method_calls] ->
      Enum.reduce(method_calls, subject, fn {:method_call, lc, [fun, args]}, subject ->
        {:fun_call, lc, [fun, [subject | args]]}
      end)
    end)
  end

  defp method_call do
    # debug :method_call,
    sequence([
      token(:colon),
      function_call()
    ])
    |> map(fn [_, {:fun_call, lc, [fun, args]}] -> {:method_call, lc, [fun, args]} end)
  end

  defp getprop_chain do
    # debug :getprop_chain,
    exclusive(
      :in_getprop_chain,
      sequence([
        sub_expr(),
        many1(getprop())
      ])
    )
    |> map(fn [subject, getters] ->
      Enum.reduce(getters, subject, fn var, subject ->
        {:getprop, nil, [subject, var]}
      end)
    end)
  end

  defp getprop do
    sequence([
      token(:.),
      token(:name)
    ])
    |> map(fn [_, var] ->
      var
    end)
  end

  defp exclusive(tag, parser) do
    fn ctx ->
      case forbidden?(ctx, tag) do
        true ->
          {:error, "exclusive mark"}

        _ ->
          ctx = forbid(ctx, tag)

          case parser.(ctx) do
            {:ok, result, rest} -> {:ok, result, unforbid(rest, tag)}
            {:error, _} = err -> err
          end
      end
    end
  end

  defp lambda_expr do
    sequence([
      token(:fn),
      token(:open_paren),
      lambda_args(),
      token(:close_paren),
      token(:arrow),
      sub_expr(),
      token(:end)
    ])
    |> map(fn [:fn, _, arg_names, _, _, expression, :end] ->
      names = Enum.map(arg_names, &elem(&1, 2))
      {:lambda, [args: names], [arg_names, expression]}
    end)
  end

  defp lambda_args do
    maybe(separated_list(plaintext_name(), token(:comma)), [])
  end

  defp function_call do
    # debug :function_call,
    sequence([
      name(),
      token(:open_paren),
      maybe(separated_list(sub_expr(), token(:comma)), []),
      token(:close_paren)
    ])
    |> map(fn [{:name, lc, fun}, _, args, _] -> {:fun_call, lc, [fun, args]} end)
  end

  defp name do
    # debug :name,
    token(:name)
  end

  defp plaintext_name do
    name()
    |> satisfy(fn {:name, meta, _name} -> not meta[:quoted] end)
  end

  defp separated_list(element_parser, separator_parser) do
    sequence([
      element_parser,
      many0(sequence([separator_parser, element_parser]))
    ])
    |> map(fn [first_element, rest] ->
      other_elements = Enum.map(rest, fn [_, element] -> element end)
      [first_element | other_elements]
    end)
  end

  defp sequence(parsers) do
    fn input ->
      case parsers do
        [] ->
          {:ok, [], input}

        [first_parser | other_parsers] ->
          with {:ok, first_term, rest} <- first_parser.(input),
               {:ok, other_terms, rest} <- sequence(other_parsers).(rest),
               do: {:ok, [first_term | other_terms], rest}
      end
    end
  end

  defp map(parser, mapper) do
    fn
      input when is_function(mapper, 1) ->
        with {:ok, term, rest} <- parser.(input),
             do: {:ok, mapper.(term), rest}
    end
  end

  defp many0(parser) do
    fn input ->
      case parser.(input) do
        {:error, _reason} ->
          {:ok, [], input}

        {:ok, first_term, rest} ->
          {:ok, other_terms, rest} = many0(parser).(rest)
          {:ok, [first_term | other_terms], rest}
      end
    end
  end

  defp maybe(parser, default) do
    fn input ->
      case parser.(input) do
        {:error, _reason} -> {:ok, default, input}
        {:ok, term, rest} -> {:ok, term, rest}
      end
    end
  end

  defp many1(parser) do
    sequence([parser, many0(parser)])
    |> map(fn [first_term, other_terms] ->
      [first_term | other_terms]
    end)
  end

  defp choice(parsers) do
    fn ctx ->
      case parsers do
        [] ->
          {:error, "no parser matched"}

        [first_parser | other_parsers] ->
          with {:error, _reason} <- first_parser.(ctx),
               do: choice(other_parsers).(ctx)
      end
    end
  end

  defp satisfy(parser, acceptor) do
    fn ctx ->
      with {:ok, term, rest} <- parser.(ctx) do
        if acceptor.(term),
          do: {:ok, term, rest},
          else: {:error, "predicate failed (token)"}
      end
    end
  end

  defp literal do
    token(:literal)
  end

  defp token(tag) do
    fn ctx ->
      case take(ctx) do
        {:ok, ^tag, ctx} -> {:ok, tag, ctx}
        {:ok, {^tag, _, _} = token, ctx} -> {:ok, token, ctx}
        {:ok, token, _} -> {:error, {"unexpected token", token}}
        :EOI -> {:error, "unexpected end of input"}
      end
    end
  end

  defp take(context(tokens: [])), do: :EOI

  defp take(context(tokens: [token | tokens], consumed: c) = ctx) do
    ctx = context(ctx, tokens: tokens, consumed: c + 1)
    {:ok, token, ctx}
  end

  defp forbid(context(marks: marks, consumed: c) = ctx, k),
    do: context(ctx, marks: Map.put(marks, {k, c}, true))

  defp unforbid(context(marks: marks, consumed: c) = ctx, k),
    do: context(ctx, marks: Map.delete(marks, {k, c}))

  defp forbidden?(context(marks: marks, consumed: c), k) do
    Map.get(marks, {k, c}, false)
  end

  @dialyzer {:no_unused, debug: 2, lvldown: 1, lvlup: 1, fail: 2, took: 3, attempt: 2}
  @doc false
  # made public for dialyzer warning about unused vars
  def debug(tag, parser) do
    fn
      context(debug: false) = ctx ->
        parser.(ctx)

      context(debug: true) = ctx ->
        case parser.(lvldown(attempt(tag, ctx))) do
          {:ok, result, new_ctx} -> took(tag, result, lvlup(new_ctx))
          {:error, reason} -> fail(ctx, reason)
        end
    end
  end

  defp lvldown(context(level: level) = ctx),
    do: context(ctx, level: level + 1)

  defp lvlup(context(level: level) = ctx),
    do: context(ctx, level: level - 1)

  defp fail(context(on_failure: f) = ctx, reason) do
    if f != nil, do: f.(ctx, reason)
    {:error, reason}
  end

  defp took(_tag, token, context(on_consumed: nil) = ctx) do
    {:ok, token, ctx}
  end

  defp took(tag, token, context(on_consumed: f) = ctx) do
    context() = new_ctx = f.(tag, token, ctx)
    {:ok, token, new_ctx}
  end

  defp attempt(tag, context(on_attempt: f) = ctx) do
    context() = new_ctx = f.(tag, ctx)
    new_ctx
  end
end
