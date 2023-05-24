defmodule Expresso.Test.Util do
  alias Expresso.Parser
  import ExUnit.Assertions

  def get_tokens(code) do
    IO.puts([IO.ANSI.yellow(), code, IO.ANSI.reset()])
    assert {:ok, value_eps, buffer} = Expresso.parse(code)

    if not Parser.empty_buffer?(buffer) do
      IO.puts([IO.ANSI.yellow(), "buffer not empty: ", IO.ANSI.reset(), elem(buffer, 1)])
    end

    IO.puts([IO.ANSI.blue(), "=> ", inspect(value_eps), IO.ANSI.reset()])
    value_eps
  end
end
