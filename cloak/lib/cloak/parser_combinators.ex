defmodule Cloak.Combinators do
  @moduledoc """
  Additional parser combinators not supplied by the `Combine` library.
  """
  import Combine.Helpers
  alias Combine.ParserState
  alias Combine.Parsers.Base

  @doc """
  Errors if the parser matches, otherwise emits an `:ok` atom.

  Can be used for a negative lookahead, to prevent some syntactical structure from
  occurring at a certain place.
  """
  @spec error_on(Base.parser, (([any]) -> String.t)) :: Base.parser
  @spec error_on(Base.parser, Base.parser, (([any]) -> String.t)) :: Base.parser
  defparser error_on(
    %ParserState{status: :ok, line: line, column: col} = state,
    parser,
    error_reporter
  ) when is_function(parser, 1)
  do
      case parser.(state) do
        %ParserState{status: :ok, results: results} ->
          %{state |
            :status => :error,
            :error => "#{error_reporter.(results)} at line #{line}, column #{col}"
          }
        _ -> %{state | results: [:ok | state.results]}
      end
  end
end
