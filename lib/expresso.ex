defmodule Expresso do
  def parse(code, opts \\ []) do
    Expresso.Parser.parse(code)
  end
end
