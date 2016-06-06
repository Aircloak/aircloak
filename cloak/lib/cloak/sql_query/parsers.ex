defmodule Cloak.SqlQuery.Parsers do
  @moduledoc """
  Additional parsers not supplied by the `Combine` library.
  """
  import Combine.Helpers
  alias Combine.ParserState

  defmodule Token do
    @moduledoc "Defines a structure which represents tokens."
    defstruct [:category, :value, :line, :column]

    @type t :: %__MODULE__{
      category: any,
      value: any,
      line: pos_integer(),
      column: pos_integer()
    }

    defimpl String.Chars do
      def to_string(token), do: "#{inspect {token.category, token.value}}"
    end
  end


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc """
  Takes a list of parser pairs, and runs the first pair where the first parser
  succeeds, emitting the result as `{first_parser_results, next_parser_results}`

  If no parser succeeds, an error is generated. You can handle this case
  specifically by providing the `{:else, parser}` pair which will always run.
  """
  @spec switch([{Base.parser | :else, Base.parser}]) :: Base.parser
  @spec switch(Base.parser, [{Base.parser | :else, Base.parser}]) :: Base.parser
  defparser switch(%ParserState{status: :ok} = state, switch_rules) do
    interpret_switch_rules(switch_rules, state)
  end

  defp interpret_switch_rules([], _state) do
    %ParserState{status: :error, error: "Expected at least one parser to succeed"}
  end
  defp interpret_switch_rules([{:else, parser} | _], state) do
    parser.(state)
  end
  defp interpret_switch_rules([{parser, next_parser} | other_rules], state) do
    case parser.(%ParserState{state | results: []}) do
      %ParserState{status: :ok, results: switch_results} = next_state ->
        case next_parser.(%ParserState{next_state | results: []}) do
          %ParserState{status: :ok, results: next_results} = final_state ->
            %ParserState{final_state |
              results: [{Enum.reverse(switch_results), Enum.reverse(next_results)} | state.results]
            }
          other -> other
        end
      _ ->
        interpret_switch_rules(other_rules, state)
    end
  end

  @doc """
  Parser which does nothing.

  This parser leaves the state unchanged. It can be useful in combination with
  the `switch` parser to emit nothing in some cases:

  ```
  switch(
    either(keyword(:foo), keyword(:bar)),
    %{
      foo: noop(),
      bar: bar_parser()
    }
  )
  ```

  If the input is a keyword `foo`, the output is `[:foo]`. If it is `bar`, the
  output is `[:bar, result_of_bar_parser]`.
  """
  @spec noop() :: Base.parser
  @spec noop(Base.parser) :: Base.parser
  defparser noop(%ParserState{status: :ok} = state) do
    state
  end

  @doc "Emits the current line and column."
  @spec position() :: Base.parser
  @spec position(Base.parser) :: Base.parser
  defparser position(%ParserState{status: :ok} = state) do
    %ParserState{state | results: [{state.line, state.column}]}
  end

  @doc "Manually increments the current line cursor as it is not done so automatically."
  @spec increment_line() :: Base.parser
  @spec increment_line(Base.parser) :: Base.parser
  defparser increment_line(%ParserState{status: :ok} = state) do
    %ParserState{state | line: (state.line + 1), column: 0}
  end

  @doc "Consumes a token of the given category."
  @spec token(any) :: Base.parser
  @spec token(Base.parser, any) :: Base.parser
  defparser token(%ParserState{status: :ok, input: input, results: results} = state, category) do
    case input do
      [] ->
        %{state | status: :error, error: "Expected #{category}, but hit end of input"}

      [%Token{category: ^category} = token | next_tokens] ->
        {next_line, next_column} =
          case next_tokens do
            [next_token | _] -> {next_token.line, next_token.column}
            [] -> {token.line, token.column}
          end
        %{state | line: next_line, column: next_column, input: next_tokens, results: [token | results]}

      [%Token{} = token | _] ->
        %{state |
          status: :error,
          error: "Unexpected token `#{to_string(token)}` at line #{state.line}, column #{state.column}}"
        }

      [other | _] -> raise "Input contained non-token #{inspect other}"
    end
  end

  @doc "Assert that there are no more tokens in the input."
  @spec end_of_tokens() :: Base.parser
  @spec end_of_tokens(Base.parser) :: Base.parser
  defparser end_of_tokens(%ParserState{status: :ok, line: line, column: column} = state) do
    case state.input do
      [] -> state
      _ -> %{state | :status => :error, :error => "Expected end of input at line #{line}, column #{column}"}
    end
  end

  @doc """
  Runs the parser and sets the error message if it fails, appending the position
  information.

  This is more flexible than `label`, because it allows you to set an arbitrary
  error message, whereas `label` can only used for `Expected ... at ...` messages.
  """
  @spec error_message(Base.parser, String.t) :: Base.parser
  @spec error_message(Base.parser, Base.parser, String.t) :: Base.parser
  defparser error_message(%ParserState{status: :ok} = state, parser, message) do
    with next_state = parser.(state),
         %ParserState{status: :error} <- next_state
    do
      %ParserState{next_state |
        error: "#{message} at line #{next_state.line}, column #{next_state.column + 1}."
      }
    end
  end
end
