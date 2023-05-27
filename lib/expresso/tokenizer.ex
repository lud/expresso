# credo:disable-for-this-file Credo.Check.Refactor.Nesting
defmodule Expresso.Tokenizer do
  alias Expresso.ParseError
  # Combinators adapted from https://gist.github.com/sasa1977/beaeb43d39b055ecb93b937123b633d5

  require Record
  Record.defrecord(:buffer, [:text, :line, :column, :level, :stack, :marks])

  defmacro debug(function) do
    {fun, arity} = __CALLER__.function

    if Mix.env() == :prod do
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

  def tokenize(input) do
    parser = expr()
    buf = buffer(input, 0, 0)

    case parse_tokens(parser, buf, []) do
      {:ok, tokens, rest} ->
        tokens = :lists.reverse(tokens)

        case empty_buffer?(consume_whitespace(rest)) do
          true ->
            {:ok, tokens}

          false ->
            {:error,
             ParseError.exception(
               message: """
               buffer not empty: #{inspect(buffer(rest, :text))}

               tokens: #{inspect(tokens)}
               """
             )}
        end

      {:error, _} = err ->
        err
    end
  end

  defp parse_tokens(parser, buf, tokens) do
    buf = consume_whitespace(buf)

    if empty_buffer?(buf) do
      {:ok, tokens, buf}
    else
      case parser.(buf) do
        {:ok, token, new_buf} ->
          parse_tokens(parser, new_buf, [token | tokens])

        {:error, reason} when is_binary(reason) ->
          {:error, ParseError.exception(message: reason)}

        {:error, reason} ->
          {:error, ParseError.exception(message: inspect(reason))}
      end
    end
  end

  defp expr() do
    choice([
      # lambda_expr(),
      # method_call_chain(),
      # getprop_chain(),
      # function_call(),
      float(),
      integer(),
      keyword(),
      name(),
      punctuation(),
      quoted_string()
    ])
  end

  defp keyword do
    choice([
      keyword(:fn),
      keyword(:end)
    ])
  end

  defp punctuation do
    token(
      choice([
        symbol('(', :open_paren),
        symbol(')', :close_paren),
        symbol(':', :colon),
        symbol(:.),
        symbol('=>', :arrow),
        symbol(',', :comma)
      ])
    )
  end

  defp name() do
    choice([
      plaintext_name() |> map(&{&1, false}),
      quoted_name() |> map(&{&1, true})
    ])
    |> map(fn {chars, quoted?}, buf ->
      {:name, [{:quoted, quoted?} | lc(buf)], to_string(chars)}
    end)
  end

  defp plaintext_name do
    sequence([
      choice([ascii_letter(), char(?_)]),
      many0(inline_key_char_num())
    ])
    |> map(fn [first, rest] -> to_string([first | rest]) end)
  end

  defp quoted_name do
    sequence([
      char(?'),
      many1(not_char(?')),
      char(?')
    ])
    |> map(fn [_, chars, _] -> chars end)
  end

  defp inline_key_char_num(), do: choice([ascii_letter(), char(?_), digit()])

  defp keyword(expected) when is_atom(expected) do
    str = to_string(expected)

    plaintext_name()
    |> token()
    |> satisfy(fn identifier -> identifier == str end)
    |> map(fn _ -> expected end)
  end

  defp symbol(chars, atom) when is_atom(atom) do
    token(sequence(Enum.map(chars, &char(&1))))
    |> map(fn _chars -> atom end)
  end

  defp symbol(atom) when is_atom(atom) do
    chars = atom |> Atom.to_string() |> String.to_charlist()
    symbol(chars, atom)
  end

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
    |> wrap_literal()
  end

  defp integer do
    sequence([
      maybe(choice([char(?-), char(?+)])),
      unsigned()
    ])
    |> map(fn chars -> chars |> :lists.flatten() |> :erlang.list_to_integer() end)
    |> wrap_literal()
  end

  defp unsigned do
    many1(digit())
  end

  defp quoted_string do
    sequence([
      char(?"),
      many0(
        choice([
          sequence([char(?\\), char(?")]) |> map(fn _ -> ?" end),
          not_char(?")
        ])
      ),
      char(?")
    ])
    |> map(fn [_, chars, _] -> to_string(chars) end)
    |> wrap_literal()
  end

  defp wrap_literal(parser) do
    map(parser, fn value, buf -> {:literal, lc(buf), value} end)
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
          {:error, "could not tokenize input"}

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
          else: {:error, "predicate failed (buffer)"}
      end
    end
  end

  defp char() do
    fn input ->
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

  if Module.get_attribute(__MODULE__, :debugging) do
    defp bufdown(buffer(level: level, stack: stack) = buf, fun),
      do: buffer(buf, level: level + 1, stack: [fun | stack])

    defp bufup(buffer(level: level) = buf) when level > 0, do: buffer(buf, level: level - 1)
    defp indentation(_, add \\ 0)
    defp indentation(buffer(level: level), add), do: indentation(level, add)
    defp indentation(level, add), do: String.duplicate("  ", level + add)
  end

  defp consume_whitespace(buf) do
    case take(buf) do
      {char, buf} when char in [?\n, ?\t, ?\s, ?\r] -> consume_whitespace(buf)
      _ -> buf
    end
  end
end
