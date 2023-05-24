defmodule Expresso.Parser do
  alias Expresso.ParseError
  # Combinators adapted from https://gist.github.com/sasa1977/beaeb43d39b055ecb93b937123b633d5

  @compile_env Mix.env()
  @dialyzer {:nowarn_function, debug: 1}

  require Record
  Record.defrecord(:buffer, [:text, :line, :column, :level, :stack, :marks])

  defmacro debug(function) do
    {fun, arity} = __CALLER__.function

    if @compile_env == :prod do
      IO.warn(
        "debug called for #{fun}/#{arity} in :prod compile environment",
        __CALLER__
      )

      quote do
        unquote(function)
      end
    else
      Module.put_attribute(__CALLER__.module, :debugging, true)

      quote do
        fn the_input ->
          text = elem(the_input, 1)
          fun = unquote(fun)

          IO.puts("#{indentation(the_input)}#{inspect(fun)} #{inspect(text)}")
          the_input = bufdown(the_input, unquote(fun))
          sub = unquote(function)
          retval = sub.(the_input)

          case retval do
            {:ok, retval, rest} ->
              IO.puts("#{indentation(rest)}=> #{inspect(fun)} = #{inspect(retval)}")
              rest = bufup(rest)
              {:ok, retval, rest}

            {:error, reason} = err when is_binary(reason) ->
              # IO.puts("#{indentation(the_input, -1)}/#{unquote(fun)} FAIL")
              err
          end
        end
      end
    end
  end

  def parse(input) do
    parser = expr()

    case parser.(buffer(input, 0, 0)) do
      {:ok, result, rest} -> {:ok, result, rest}
      {:error, reason} when is_binary(reason) -> {:error, ParseError.exception(message: reason)}
      {:error, reason} -> {:error, ParseError.exception(message: inspect(reason))}
    end
  end

  defp lazy(combinator) do
    fn input ->
      parser = combinator.()
      parser.(input)
    end
  end

  defp expr() do
    choice([
      method_call_chain(),
      function_call(),
      float(),
      data_path(),
      integer()
    ])
  end

  defp sub_expr, do: lazy(fn -> expr() end)

  defp method_call_chain do
    exclusive(
      :in_method_call_chain,
      sequence([
        sub_expr(),
        many1(method_call())
      ])
    )
    |> map(fn [subject, method_calls] ->
      Enum.reduce(method_calls, subject, fn {:method_call, nil, [fun, args]}, subject ->
        {:fun_call, nil, [fun, [subject | args]]}
      end)
    end)
  end

  defp method_call do
    exclusive(
      :in_method_call,
      sequence([
        char(?:),
        function_call()
      ])
      |> map(fn [_, {:fun_call, nil, [fun, args]}] ->
        {:method_call, nil, [fun, args]}
      end)
    )
  end

  defp exclusive(tag, parser) do
    fn buf ->
      case forbidden?(buf, tag) do
        true ->
          {:error, "exclusive mark"}

        _ ->
          buf = forbid(buf, tag)

          case parser.(buf) do
            {:ok, result, rest} ->
              {:ok, result, unforbid(rest, tag)}

            {:error, _} = err ->
              err
          end
      end
    end
  end

  defp function_call do
    sequence([
      inline_indentifier(),
      char(?(),
      arguments(),
      char(?))
    ])
    |> map(fn [fun, _, args, _] -> {:fun_call, nil, [fun, args]} end)
  end

  defp arguments do
    separated_list(token(sub_expr()), char(?,))
  end

  defp data_path do
    sequence([
      identifier(),
      many0(sequence([char(?.), identifier()]))
    ])
    |> map(fn [first, rest], buf ->
      other_elements = Enum.map(rest, fn [_, element] -> element end)
      {:dpath, lc(buf), [first | other_elements]}
    end)
  end

  defp identifier() do
    choice([
      inline_indentifier(),
      sequence([
        char(?'),
        many1(not_char(?')),
        char(?')
      ])
      |> map(fn [_, chars, _] -> chars end)
    ])
    |> map(fn chars ->
      to_string(chars)
    end)
  end

  defp inline_indentifier do
    sequence([
      choice([ascii_letter(), char(?_)]),
      many0(inline_key_char_num())
    ])
    |> map(fn [first, rest] ->
      to_string([first | rest])
    end)
  end

  defp inline_key_char_num(), do: choice([ascii_letter(), char(?_), digit()])

  defp float do
    choice([
      sequence([
        # integer part
        maybe(choice([char(?-), char(?+)])),
        unsigned(),
        # Dot
        char(?.),
        # Fractional part
        unsigned(),
        # scientific notation part
        choice([char(?e), char(?E)]),
        maybe(choice([char(?-), char(?+)])),
        unsigned()
      ]),
      sequence([
        # integer part
        maybe(choice([char(?-), char(?+)])),
        unsigned(),
        # Dot
        char(?.),
        # Fractional part
        unsigned()
      ])
    ])
    |> map(fn chars -> chars |> :lists.flatten() |> :erlang.list_to_float() end)
  end

  defp integer do
    sequence([
      maybe(choice([char(?-), char(?+)])),
      unsigned()
    ])
    |> map(fn chars -> chars |> :lists.flatten() |> :erlang.list_to_integer() end)
  end

  defp unsigned do
    many1(digit())
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

  defp token(parser) do
    whitespace = choice([char(?\s), char(?\n), char(?\t), char(?\r)])

    sequence([
      many0(whitespace),
      parser,
      many0(whitespace)
    ])
    |> map(fn [_lws, term, _rws] -> term end)
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

      input when is_function(mapper, 2) ->
        with {:ok, term, rest} <- parser.(input),
             do: {:ok, mapper.(term, input), rest}
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

  defp maybe(parser) do
    fn input ->
      case parser.(input) do
        {:error, _reason} -> {:ok, [], input}
        {:ok, term, rest} -> {:ok, [term], rest}
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
    fn input ->
      case parsers do
        [] ->
          {:error, "no parser suceeded"}

        [first_parser | other_parsers] ->
          with {:error, _reason} <- first_parser.(input),
               do: choice(other_parsers).(input)
      end
    end
  end

  defp digit(), do: satisfy(char(), fn char -> char in ?0..?9 end)
  defp ascii_letter(), do: satisfy(char(), fn char -> char in ?A..?Z or char in ?a..?z end)

  defp char(expected), do: satisfy(char(), fn char -> char == expected end)
  defp not_char(rejected), do: satisfy(char(), fn char -> char != rejected end)

  defp satisfy(parser, acceptor) do
    fn input ->
      with {:ok, term, rest} <- parser.(input) do
        if acceptor.(term),
          do: {:ok, term, rest},
          else: {:error, "term rejected"}
      end
    end
  end

  defp char() do
    fn input ->
      # input |> IO.inspect(label: ~S/char/)

      case take(input) do
        :EOI -> {:error, "unexpected end of input"}
        {char, buf} -> {:ok, char, buf}
      end
    end
  end

  def buffer(text, line, column) do
    buffer(text: text, line: line, column: column, level: 0, stack: [], marks: %{})
  end

  def buffer(buf, text, line, column) do
    buffer(buf, text: text, line: line, column: column)
  end

  defp take(buffer(text: text, line: line, column: column) = buf) do
    case text do
      <<?\n, rest::binary>> -> {?\n, buffer(buf, rest, line + 1, 0)}
      <<char::utf8, rest::binary>> -> {char, buffer(buf, rest, line, column + 1)}
      "" -> :EOI
    end
  end

  defp lc(buffer(line: line, column: column)), do: [line: line, column: column]

  def empty_buffer?(buffer(text: text)), do: text == ""

  defp forbid(buffer(marks: marks, line: l, column: c) = buf, k),
    do: buffer(buf, marks: Map.put(marks, {k, l, c}, true))

  defp unforbid(buffer(marks: marks, line: l, column: c) = buf, k),
    do: buffer(buf, marks: Map.delete(marks, {k, l, c}))

  defp forbidden?(buffer(marks: marks, line: l, column: c), k),
    do: Map.get(marks, {k, l, c}, false)

  if Module.get_attribute(__MODULE__, :debugging) do
    # defp debug_stack(buffer(stack: stack)),
    #   do: stack |> Enum.reverse() |> Enum.map_join("/", &to_string/1)

    defp bufdown(buffer(level: level, stack: stack) = buf, fun),
      do: buffer(buf, level: level + 1, stack: [fun | stack])

    defp bufup(buffer(level: level) = buf) when level > 0, do: buffer(buf, level: level - 1)
    defp indentation(_, add \\ 0)
    defp indentation(buffer(level: level), add), do: indentation(level, add)
    defp indentation(level, add), do: String.duplicate("  ", level + add)
  end
end
