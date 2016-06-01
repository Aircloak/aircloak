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
  Runs the first parser, then based on its result runs the corresponding
  parser from the supplied map.

  The parser emits a tuple in form of `{switch_parser_output, selected_parser_output}`.

  Example:

  ```
  switch(
    either(keyword(:foo), keyword(:bar)),
    %{
      foo: foo_parser(),
      bar: bar_parser()
    }
  )
  ```

  The output will be `{:foo, output_of_foo_parser}` or `{:bar, output_of_bar_parser}`.

  If the parser for the corresponding term has not been supplied, a run-time error
  will be raised.
  ```
  """
  @spec switch(Base.parser, %{any => Base.parser}) :: Base.parser
  @spec switch(Base.parser, Base.parser, %{any => Base.parser}) :: Base.parser
  defparser switch(
    %ParserState{status: :ok} = state,
    switch_parser,
    switch_map
  ) do
    with switch_state = switch_parser.(state),
         %ParserState{status: :ok, results: [switch_result | rest]} <- switch_state,
         next_parser = Map.fetch!(switch_map, switch_result),
         final_state = next_parser.(%ParserState{switch_state | results: []}),
         %ParserState{status: :ok, results: new_results} <- final_state
    do
      %ParserState{final_state | results: [{switch_result, new_results} | rest]}
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

  @doc "Consumes a token of the given category."
  @spec token(any) :: Base.parser
  @spec token(Base.parser, any) :: Base.parser
  defparser token(%ParserState{status: :ok, input: input, results: results} = state, category) do
    if Enum.empty?(input) do
      %{state | status: :error, error: "Expected #{category}, but hit end of input"}
    else
      case hd(input) do
        %Token{category: ^category} = token ->
          %{state |
            line: token.line, column: token.column, input: tl(input), results: [token | results]
          }
        %Token{} = token ->
          %{state |
            line: token.line, column: token.column, status: :error,
            error: "Unexpected token `#{to_string(token)}` at line #{token.line}, column #{token.column}}"
          }
        other -> raise "Input contained non-token #{inspect other}"
      end
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
  Runs the `if` parser, and depending on its success, either the `then` or `else` parser.

  This parser can be more useful than built-in `option` to get better error reporting.

  One example is customizing the error string:

  ```
  if_then_else(some_parser(), noop(), fail("custom error message"))
  ```

  In this example, if `some_parser` succeeds, we do nothing. Otherwise, we fail
  with a custom error message.

  Another example is reporting an error on an optional construct. Let's say we want to
  parse an optional where clause. Using a naive `option(where_parser())` won't
  report a proper error in the `where` clause, since `option` will simply ignore
  any reported error.

  Using this parser, you can first check for the keyword presence, and then
  parse the rest of the expression:

  ```
  if_then_else(keyword(:where), rest_of_where_parser(), noop())
  ```

  If the `where` keyword is provided, we parse the rest, and report a proper error.
  Otherwise, we'll just run the `noop()` parser, leaving the state unchanged.

  Notice that `then_parser` is running on the state produced by the `if_parser`.
  This means that results produced by the `if_parser` are already on the stack.
  If you want to group the results of both parser, you can use `group/1`.
  """
  @spec if_then_else(Base.parser, Base.parser, Base.parser) :: Base.parser
  @spec if_then_else(Base.parser, Base.parser, Base.parser, Base.parser) :: Base.parser
  defparser if_then_else(%ParserState{status: :ok} = state, if_parser, then_parser, else_parser) do
    case if_parser.(state) do
      %ParserState{status: :ok} = next_state -> then_parser.(next_state)
      _ -> else_parser.(state)
    end
  end

  @doc """
  Groups the last `n` results into a tuple.

  This parser can be useful with `if_then_else/3` to retroactively group the
  results of `if_parser` and `then_parser`:

  ```
  if_then_else(keyword(:where), where_expressions() |> group(2), noop())
  ```

  In this example, if the keyword `where` is provided, the output will be
  `{output_of_keyword_parser, output_of_where_expressions_parser}`
  """
  @spec group(pos_integer) :: Base.parser
  @spec group(Base.parser, pos_integer) :: Base.parser
  defparser group(%ParserState{status: :ok, results: results} = state, n) do
    {result_group, rest} = Enum.split(results, n)
    %ParserState{state | results: [List.to_tuple(Enum.reverse(result_group)) | rest]}
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
