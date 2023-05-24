defmodule Expresso.EvalError do
  defexception [:tag, :meta]

  @identifier_first_chars String.graphemes(
                            "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_"
                          )
  @identifier_last_chars String.graphemes(
                           "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_0123456789"
                         )

  def invalid_path(path, data) do
    exception(:invalid_path, %{path: path, map?: is_map(data), data: data})
  end

  def exception(tag, meta) do
    exception(tag: tag, meta: meta)
  end

  def message(%{tag: tag, meta: meta}), do: message(tag, meta)

  defp message(:invalid_path, %{path: path, map?: map?, data: data}) do
    sub = if(map?, do: "key not found", else: "data is not a map")
    "could not retrieve path #{format_path(path)} from data: #{sub}"
  end

  defp format_path(path) do
    Enum.map_join(path, ".", &format_path_item/1)
  end

  defp format_path_item(item) do
    [first | rest] = String.graphemes(item)

    if first in @identifier_first_chars and Enum.all?(rest, &(&1 in @identifier_last_chars)) do
      item
    else
      "'" <> item <> "'"
    end
  end
end
