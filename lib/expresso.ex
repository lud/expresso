defmodule Expresso do
  alias Expresso.Completions
  alias Expresso.Tokenizer
  alias Expresso.EvalError
  alias Expresso.ParseError
  alias Expresso.Parser
  alias Expresso.VM

  def tokenize(code) do
    Tokenizer.tokenize(code)
  end

  def tokenize!(code) do
    case tokenize(code) do
      {:ok, tokens} -> tokens
      {:error, %ParseError{} = e} -> raise e
    end
  end

  def parse_tokens(code, opts \\ []) do
    Parser.parse_tokens(code, opts)
  end

  def eval_string(code, data \\ %{}, opts \\ []) do
    with {:ok, tokens} <- tokenize(code),
         {:ok, ast} <- parse_tokens(tokens, opts),
         {:ok, value, state} <- eval_ast(ast, data, opts) do
      {:ok, value, state}
    else
      {:error, %EvalError{} = e} -> {:error, EvalError.with_source(e, code)}
      {:error, e} -> {:error, e}
    end
  end

  def eval_ast(ast, data \\ %{}, opts \\ []) do
    VM.run(ast, data, opts)
  end

  def get_completions(code, data \\ %{}) do
    with {:ok, tokens} <- tokenize(code) do
      {:ok, Completions.from_tokens(tokens, data)}
    end
  end
end
