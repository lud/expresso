defmodule Expresso.Parser do
  # Combinators adapted from https://gist.github.com/sasa1977/beaeb43d39b055ecb93b937123b633d5

  @compile_env Mix.env()
  @dialyzer {:nowarn_function, debug: 1}

  defmacro debug({:fn, _, [{:->, _, [[arg], body]} | []]}) do
    if @compile_env == :prod do
      quote do
        fn unquote(arg) -> unquote(body) end
      end
    else
      {fun, arity} = __CALLER__.function

      quote do
        fn the_input ->
          the_input = bufdown(the_input, unquote(fun))
          unquote(arg) = the_input
          the_input |> IO.inspect(label: ~S/the_input/)

          case unquote(body) do
            {:ok, retval, rest} ->
              rest |> IO.inspect(label: ~S/rest/)
              IO.puts("#{indentation(the_input)}#{debug_stack(rest)}")
              IO.puts("#{indentation(rest)}#{inspect(retval)}")
              {:ok, retval, bufup(rest)}

            {:error, reason} = err when is_binary(reason) ->
              err
          end
        end
      end
    end
  end

  defmacro debug({:fn, _, [_, _ | []]}) do
    raise "debug macro not supported for multiple function clauses or multiple arguments"
  end

  defmacro debug({:fn, _, other}) do
    raise "debug macro bad call: #{inspect(other, pretty: true)}"
  end

  defp return(v, rest) do
    {:ok, v, rest}
  end

  def parse(input) do
    parser = expr()
    parser.(buffer(input, 0, 0))
  end

  defp lazy(combinator) do
    fn input ->
      parser = combinator.()
      parser.(input)
    end
  end

  defp expr() do
    choice([
      float(),
      data_path(),
      integer()
    ])
  end

  defp data_path do
    sequence([
      identifier(),
      many0(sequence([char(?.), identifier()]))
    ])
    |> map(fn [first, rest] ->
      other_elements = Enum.map(rest, fn [_, element] -> element end)
      {:dpath, nil, [first | other_elements]}
    end)
  end

  defp identifier() do
    choice([
      sequence([
        choice([ascii_letter(), char(?_)]),
        many1(inline_key_char_num())
      ])
      |> map(fn x ->
        x |> IO.inspect(label: ~S/x/)
        x
      end),
      sequence([
        char(?'),
        many1(not_char(?')),
        char(?')
      ])
      |> map(fn [_, chars, _] ->
        chars |> IO.inspect(label: ~S/quoted identifier/)
        chars
      end)
    ])
    |> map(fn chars ->
      chars |> IO.inspect(label: ~S/identifier/)
      to_string(chars)
    end)
  end

  defp inline_key_char(), do: choice([ascii_letter(), char(?_)])
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
    debug(fn input ->
      case parsers do
        [] ->
          {:ok, [], input}

        [first_parser | other_parsers] ->
          with {:ok, first_term, rest} <- first_parser.(input),
               {:ok, other_terms, rest} <- sequence(other_parsers).(rest),
               do: {:ok, [first_term | other_terms], rest}
      end
    end)
  end

  defp map(parser, mapper) do
    debug(fn
      input ->
        with {:ok, term, rest} <- parser.(input),
             do: return(mapper.(term), rest)
    end)
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
    debug(fn input ->
      case parsers do
        [] ->
          {:error, "no parser suceeded"}

        [first_parser | other_parsers] ->
          with {:error, _reason} <- first_parser.(input),
               do: choice(other_parsers).(input)
      end
    end)
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

  require Record
  Record.defrecord(:buffer, [:text, :line, :column, :level, :stack])

  def buffer(text, line, column) do
    buffer(text: text, line: line, column: column, level: 0, stack: [])
  end

  defp take(buffer(text: text, line: line, column: column)) do
    case text do
      <<?\n, rest::binary>> -> {?\n, buffer(rest, line + 1, 0)}
      <<char::utf8, rest::binary>> -> {char, buffer(rest, line, column + 1)}
      "" -> :EOI
    end
  end

  defp bufdown(buffer(level: level, stack: stack) = buf, fun),
    do: buffer(buf, level: level + 1, stack: [fun | stack])

  defp bufup(buffer(level: level) = buf), do: buffer(buf, level: level - 1)

  defp indentation(buffer(level: level)) when level < 0,
    do: "==" <> Integer.to_string(level) <> "=="

  defp indentation(buffer(level: level)), do: String.duplicate("  ", level)
  def empty_buffer?(buffer(text: text)), do: text == ""

  defp debug_stack(buffer(stack: stack)),
    do: stack |> Enum.reverse() |> Enum.map_join("/", &to_string/1)
end
