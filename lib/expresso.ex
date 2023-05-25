defmodule Expresso do
  alias Expresso.EvalError
  alias Expresso.ParseError
  alias Expresso.Parser
  alias Expresso.VM

  def parse(code, _opts \\ []) do
    Parser.parse(code)
  end

  def parse!(code, opts \\ []) do
    case parse(code, opts) do
      {:ok, ast} -> ast
      {:error, %ParseError{} = e} -> raise e
    end
  end

  def eval_string(code, opts \\ []) do
    case parse(code, opts) do
      {:error, reason} ->
        {:error, reason}

      {:ok, ast} ->
        case eval_ast(ast, opts) do
          {:ok, value, state} -> {:ok, value, state}
          {:error, %EvalError{} = e} -> {:error, EvalError.with_source(e, code)}
        end
    end
  end

  def eval_ast(ast, opts \\ []) do
    VM.run(ast, opts)
  end
end
