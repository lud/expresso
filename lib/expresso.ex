defmodule Expresso do
  alias Expresso.EvalError
  alias Expresso.ParseError
  alias Expresso.Parser
  alias Expresso.VM

  def parse(code, opts \\ []) do
    Parser.parse(code)
  end

  def parse!(code, opts \\ []) do
    case parse(code, opts) do
      {:ok, ast} -> ast
      {:error, %ParseError{} = e} -> raise e
    end
  end

  def eval_string(code, opts \\ []) do
    with {:ok, ast} <- parse(code, opts) do
      Interpreter.run(ast, opts)
    else
      {:error, %EvalError{} = e} -> {:error, EvalError.with_source(e, code)}
      {:error, reason} -> {:error, reason}
    end
  end
end
