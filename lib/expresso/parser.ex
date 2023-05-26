# credo:disable-for-this-file Credo.Check.Refactor.Nesting
defmodule Expresso.Parser do
  alias Expresso.ParseError
  # Combinators adapted from https://gist.github.com/sasa1977/beaeb43d39b055ecb93b937123b633d5

  require Record

  Record.defrecord(:buffer, [
    :text,
    :line,
    :column,
    :level,
    :scopes,
    :preserve_scopes,
    :locks,
    :on_eoi
  ])

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
        fn the_buf ->
          text = elem(the_buf, 1)
          fun = unquote(fun)

          IO.puts("#{indentation(the_buf)}#{inspect(fun)} #{inspect(text)}")
          the_buf = bufdown(the_buf, unquote(fun))
          sub = unquote(function)
          retval = sub.(the_buf)

          case retval do
            {:ok, retval, rest} ->
              IO.puts("#{indentation(rest)}=> #{inspect(fun)} = #{inspect(retval)}")
              rest = bufup(rest)
              {:ok, retval, rest}

            {:error, reason} = err when is_binary(reason) ->
              # IO.puts("#{indentation(the_buf, -1)}/#{unquote(fun)} FAIL")
              err
          end
        end
      end
    end
  end

  def parse(input, opts \\ []) do
    on_eoi = Keyword.get(opts, :on_eoi, nil)
    buf = consume_whitespace(new_buffer(input, 0, 0, on_eoi))
    parser = expr()

    case parser.(buf) do
      {:ok, result, rest} ->
        case empty_buffer?(consume_whitespace(rest)) do
          true ->
            {:ok, result}

          false ->
            {:error,
             ParseError.exception(
               message: """
               buffer not empty: #{inspect(buffer(rest, :text))}

               tokens: #{inspect(result)}
               """
             )}
        end

      {:error, reason} when is_binary(reason) ->
        {:error, ParseError.exception(message: reason)}

      {:error, reason} ->
        {:error, ParseError.exception(message: inspect(reason))}
    end
  end

  defp reg_scope(tag, parser) do
    fn buf ->
      case parser.(buf) do
        {:ok, result, rest} -> {:ok, result, put_scope(rest, tag, result)}
        {:error, reason} -> {:error, reason}
      end
    end
  end

  defp reg_scope_name(tag, parser) do
    fn buf ->
      case parser.(buf) do
        {:ok, {:name, _, name} = result, rest} -> {:ok, result, put_scope(rest, tag, name)}
        {:error, reason} -> {:error, reason}
      end
    end
  end

  defp lazy(combinator) do
    fn buf ->
      parser = combinator.()
      parser.(buf)
    end
  end

  defp expr() do
    choice([
      lambda_expr(),
      method_call_chain(),
      getprop_chain(),
      function_call(),
      float(),
      integer(),
      root_var(),
      quoted_string()
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
      Enum.reduce(method_calls, subject, fn {:method_call, lc, [fun, args]}, subject ->
        {:fun_call, lc, [fun, [subject | args]]}
      end)
    end)
  end

  defp getprop_chain do
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

  defp method_call do
    sequence([
      token(char(?:)),
      function_call()
    ])
    |> map(fn [_, {:fun_call, lc, [fun, args]}] ->
      {:method_call, lc, [fun, args]}
    end)
  end

  defp getprop do
    sequence([
      char(?.),
      reg_scope_name(:getprop, name())
    ])
    |> map(fn [_, name] -> name end)
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

  defp lambda_expr do
    sequence([
      keyword(:fn),
      char(?(),
      lambda_args(),
      char(?)),
      symbol('=>'),
      sub_expr(),
      keyword(:end)
    ])
    |> map(fn [:fn, _, arg_names, _, _, expression, :end], buf ->
      {:lambda, lc(buf), [arg_names, expression]}
    end)
  end

  defp lambda_args do
    separated_list(
      token(plaintext_name() |> map(fn key, buf -> {:arg, lc(buf), key} end)),
      char(?,)
    )
  end

  defp function_call do
    sequence([
      plaintext_name(),
      char(?(),
      choice([
        separated_list(token(sub_expr()), char(?,)),
        many0(whitespace())
      ]),
      char(?))
    ])
    |> map(fn [fun, _, args, _], buf -> {:fun_call, lc(buf), [fun, args]} end)
  end

  defp root_var do
    reg_scope_name(:variable, name())
  end

  defp name do
    choice([
      plaintext_name(),
      quoted_name()
    ])
    |> map(fn chars, buf -> {:name, lc(buf), to_string(chars)} end)
  end

  defp plaintext_name do
    sequence([
      choice([ascii_letter(), char(?_)]),
      many0(inline_key_char_num())
    ])
    |> map(fn [first, rest] ->
      to_string([first | rest])
    end)
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
    |> satisfy(fn plain -> plain == str end)
    |> map(fn _ -> expected end)
  end

  defp symbol(chars) when is_list(chars) do
    token(sequence(Enum.map(chars, &char(&1))))
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
    fn buf ->
      # buf = preserve_scopes(buf)

      case do_sequence(buf, parsers) do
        # {:ok, terms, rest} -> {:ok, terms, unpreserve_scopes(rest)}
        {:ok, terms, rest} -> {:ok, terms, rest}
        {:error, _} = err -> err
      end
    end
  end

  defp do_sequence(buf, parsers) do
    case parsers do
      [] ->
        {:ok, [], buf}

      [first_parser | other_parsers] ->
        with {:ok, first_term, rest} <- first_parser.(buf),
             {:ok, other_terms, rest} <- do_sequence(rest, other_parsers),
             do: {:ok, [first_term | other_terms], rest}
    end
  end

  defp map(parser, mapper) do
    fn
      buf when is_function(mapper, 1) ->
        with {:ok, term, rest} <- parser.(buf),
             do: {:ok, mapper.(term), rest}

      buf when is_function(mapper, 2) ->
        with {:ok, term, rest} <- parser.(buf),
             do: {:ok, mapper.(term, buf), rest}
    end
  end

  defp many0(parser) do
    fn buf ->
      case parser.(buf) do
        {:error, _reason} ->
          {:ok, [], buf}

        {:ok, first_term, rest} ->
          {:ok, other_terms, rest} = many0(parser).(rest)
          {:ok, [first_term | other_terms], rest}
      end
    end
  end

  defp maybe(parser) do
    fn buf ->
      case parser.(buf) do
        {:error, _reason} -> {:ok, [], buf}
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
    fn buf ->
      case parsers do
        [] ->
          {:error, "no parser suceeded"}

        [first_parser | other_parsers] ->
          with {:error, _reason} <- first_parser.(buf),
               do: choice(other_parsers).(buf)
      end
    end
  end

  defp digit(), do: satisfy(char(), fn char -> char in ?0..?9 end)
  defp ascii_letter(), do: satisfy(char(), fn char -> char in ?A..?Z or char in ?a..?z end)
  defp whitespace(), do: satisfy(char(), fn char -> char in [?\s, ?\n, ?\t, ?\r] end)

  defp char(expected), do: satisfy(char(), fn char -> char == expected end)
  defp not_char(rejected), do: satisfy(char(), fn char -> char != rejected end)

  defp satisfy(parser, acceptor) do
    fn buf ->
      with {:ok, term, rest} <- parser.(buf) do
        if acceptor.(term),
          do: {:ok, term, rest},
          else: {:error, "term rejected"}
      end
    end
  end

  defp char() do
    fn buf ->
      case take(buf) do
        :EOI -> {:error, "unexpected end of input"}
        {char, buf} -> {:ok, char, buf}
      end
    end
  end

  def new_buffer(text, line, column, on_eoi) do
    # end

    # def buffer(text, line, column) do
    buffer(
      text: text,
      line: line,
      column: column,
      level: 0,
      preserve_scopes: 0,
      scopes: [],
      locks: %{},
      on_eoi: on_eoi
    )
  end

  defp take(buffer(text: text, line: line, column: column, on_eoi: on_eoi, scopes: scopes) = buf) do
    case text do
      <<?\n, rest::binary>> ->
        {?\n, buffer(buf, text: rest, line: line + 1, column: 0)}

      <<char::utf8, rest::binary>> ->
        {char, buffer(buf, text: rest, line: line, column: column + 1)}

      "" ->
        scopes |> IO.inspect(label: ~S/scopes on EOI/)
        apply_hook(on_eoi, buf)
        :EOI
    end
  end

  defp apply_hook(nil, _), do: :ok

  defp apply_hook(f, buf) do
    f |> IO.inspect(label: ~S/apply_hook/)
    f.(buf)
  end

  defp lc(buffer(line: line, column: column)), do: [line: line, column: column]

  def empty_buffer?(buffer(text: text)), do: text == ""

  defp forbid(buffer(locks: locks, line: l, column: c) = buf, k),
    do: buffer(buf, locks: Map.put(locks, {k, l, c}, true))

  defp unforbid(buffer(locks: locks, line: l, column: c) = buf, k),
    do: buffer(buf, locks: Map.delete(locks, {k, l, c}))

  defp forbidden?(buffer(locks: locks, line: l, column: c), k),
    do: Map.get(locks, {k, l, c}, false)

  # @dialyzer {:nowarn_function, {__new__type_check_return_type_wrapper__: 1}

  defp bufdown(buffer(level: level) = buf, _fun),
    do: buffer(buf, level: level + 1)

  defp bufup(buffer(level: level) = buf) when level > 0, do: buffer(buf, level: level - 1)
  defp indentation(_, add \\ 0)
  defp indentation(buffer(level: level), add), do: indentation(level, add)
  defp indentation(level, add), do: String.duplicate("  ", level + add)

  defp consume_whitespace(buf) do
    case take(buf) do
      {char, buf} when char in [?\n, ?\t, ?\s, ?\r] -> consume_whitespace(buf)
      _ -> buf
    end
  end

  def buffer_scope(buffer(scopes: scope)), do: scope

  defp put_scope(buffer(text: text) = buf, scope) do
    put_scope(buf, scope, text)
  end

  defp put_scope(buffer(text: text, scopes: scopes) = buf, scope, value) do
    IO.puts("enter scope #{inspect(scope)}: #{inspect(value)}, text: #{inspect(text)}")
    new_scopes = [{scope, value} | scopes]
    new_scopes |> IO.inspect(label: ~S/new_scopes/)
    buffer(buf, scopes: new_scopes)
  end

  # preserve_scopes is not the number of scopes to preserve, it is a semaphore
  # that is incremented by each preserving sequence.
  defp drop_scope(buffer(preserve_scopes: 0, scopes: [left | scopes], text: text) = buf) do
    IO.puts("leave scope #{inspect(left)}, text: #{inspect(text)}")
    buffer(buf, scopes: scopes)
  end

  defp drop_scope(buffer(scopes: [left | _], text: text) = buf) do
    IO.puts("keep scope #{inspect(left)}, text: #{inspect(text)}")
    buf
  end

  defp preserve_scopes(buffer(preserve_scopes: depth, scopes: scopes) = buf) do
    new_depth = depth + 1
    buffer(buf, preserve_scopes: new_depth, scopes: [new_depth | scopes])
  end

  defp unpreserve_scopes(buffer(preserve_scopes: depth, scopes: scopes) = buf) do
    new_depth = depth - 1
    buffer(buf, preserve_scopes: new_depth, scopes: cleanup_scopes(scopes, depth))
  end

  # drop all scopes until it finds the given scope, then drop that scope too and
  # return the rest
  defp cleanup_scopes([same | t], same), do: t

  defp cleanup_scopes([left | t], milestone) do
    IO.puts("remove scope #{inspect(left)}")
    cleanup_scopes(t, milestone)
  end
end
