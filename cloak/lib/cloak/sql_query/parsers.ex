defmodule Cloak.SqlQuery.Parsers do
  @moduledoc """
  Additional parsers not supplied by the `Combine` library.
  """
  import Combine.Helpers
  alias Combine.ParserState


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
         %ParserState{status: :ok, results: [switch_result | rest] = results} <- switch_state,
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
  @spec noop(t :: Base.parser) :: Base.parser
  defparser noop(%ParserState{status: :ok} = state) do
    state
  end
end
