defmodule Expresso.EvalError do
  defexception [:tag, :meta, :line, :column, :source]

  @identifier_first_chars String.graphemes(
                            "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_"
                          )
  @identifier_last_chars String.graphemes(
                           "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_0123456789"
                         )

  # -- Custom errors ----------------------------------------------------------

  def key_error({:var, lc, key}, data) do
    build(:key_error, lc, %{key: key, map?: is_map(data), data: data})
  end

  def arg_error({:fun_call, lc, [fun, args]}, arg, arg_lc, arg_num, errmsg) do
    build(:arg_error, arg_lc || lc, %{
      fun: fun,
      args: args,
      arg: arg,
      arg_num: arg_num,
      errmsg: errmsg
    })
  end

  def undefined_function({:fun_call, lc, [fun, args]}) do
    build(:undefined_function, lc, %{fun: fun, args: args})
  end

  # -- Generic error builder --------------------------------------------------

  defp build(tag, lc, meta) when is_list(lc) do
    line = Keyword.get(lc, :line)
    column = Keyword.get(lc, :column)
    exception(tag: tag, meta: meta, line: line, column: column)
  end

  defp build(tag, nil, meta) do
    build(tag, [], meta)
  end

  # -- Generic Messages -------------------------------------------------------

  def with_source(%{source: _} = e, code), do: %{e | source: code}

  def message(%{source: code, line: line, column: column} = e)
      when is_binary(code) and is_integer(line) and is_integer(column) do
    :erlang.iolist_to_binary([
      message(with_source(e, nil)),
      format_source_loc(code, line, column)
    ])
  end

  def message(%{tag: tag, meta: meta}), do: message(tag, meta)

  # -- Specific messages ------------------------------------------------------

  defp message(:key_error, %{key: key, map?: true}) do
    "could not find key `#{format_key(key)}` from data"
  end

  defp message(:key_error, %{key: key, map?: false}) do
    "cannot dereference key `#{format_key(key)}`, data is not an object"
  end

  defp message(:arg_error, %{fun: fun, arg_num: arg_num, errmsg: errmsg}) do
    "invalid #{nth(arg_num)} argument for function `#{fun}`, #{errmsg}"
  end

  defp message(:undefined_function, %{fun: fun}) do
    "function `#{fun}` is not defined"
  end

  def format_source_loc(code, line, column) do
    lines = String.split(code, "\n")

    case Enum.at(lines, line) do
      nil -> ""
      line -> ["\n\n", line, "\n", String.duplicate(" ", column), "^ error occured here\n"]
    end
  end

  defp format_key(item) do
    [first | rest] = String.graphemes(item)

    if first in @identifier_first_chars and Enum.all?(rest, &(&1 in @identifier_last_chars)) do
      item
    else
      "'" <> item <> "'"
    end
  end

  defp nth(1), do: "1st"
  defp nth(2), do: "2nd"
  defp nth(3), do: "3rd"
  defp nth(n), do: "#{n}th"
end
