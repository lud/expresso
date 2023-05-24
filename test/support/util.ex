defmodule Expresso.Test.Util do
  alias Expresso.Parser
  import ExUnit.Assertions

  def get_tokens(code, opts \\ []) do
    print? = opts[:print]

    if print? do
      IO.puts([IO.ANSI.yellow(), code, IO.ANSI.reset()])
    end

    value_eps = Expresso.parse!(code)

    if print? do
      IO.puts([IO.ANSI.blue(), "=> ", inspect(value_eps), IO.ANSI.reset()])
    end

    value_eps
  end
end
