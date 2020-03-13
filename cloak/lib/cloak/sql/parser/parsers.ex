defmodule Cloak.Sql.Parser.Parsers do
  @moduledoc """
  Additional parsers not supplied by the `Combine` library.
  """
  import Combine.Helpers
  alias Combine.Parsers.Base
  alias Combine.ParserState

  defmodule Token do
    @moduledoc "Defines a structure which represents tokens."
    defstruct [:category, :value, :offset, :line, :column]

    @type t :: %__MODULE__{
            category: any,
            value: any,
            offset: non_neg_integer(),
            line: pos_integer(),
            column: non_neg_integer()
          }

    defimpl String.Chars do
      def to_string(token), do: "#{inspect({token.category, token.value})}"
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
  @spec switch(Combine.previous_parser(), [{Combine.parser() | :else, Combine.parser()}]) :: Combine.parser()
  defparser switch(%ParserState{status: :ok, line: line, column: column} = state, switch_rules) do
    interpret_switch_rules(
      switch_rules,
      state,
      "Expected at least one parser to succeed at line #{line}, column #{column}."
    )
  end

  defp interpret_switch_rules([], _state, deepest_error) do
    %ParserState{status: :error, error: deepest_error}
  end

  defp interpret_switch_rules([{:else, parser} | _], state, _deepest_error) do
    parser.(state)
  end

  defp interpret_switch_rules([{parser, next_parser} | other_rules], state, deepest_error) do
    case parser.(%ParserState{state | results: []}) do
      %ParserState{status: :ok, results: switch_results} = next_state ->
        case next_parser.(%ParserState{next_state | results: []}) do
          %ParserState{status: :ok, results: next_results} = final_state ->
            %ParserState{
              final_state
              | results: [
                  {Enum.reverse(switch_results), Enum.reverse(next_results)} | state.results
                ]
            }

          other ->
            other
        end

      %ParserState{error: error} ->
        interpret_switch_rules(other_rules, state, deeper_error(deepest_error, error))
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
  @spec noop(Combine.previous_parser()) :: Combine.parser()
  defparser noop(%ParserState{status: :ok} = state) do
    state
  end

  @doc "Emits the current line and column."
  @spec position(Combine.previous_parser()) :: Combine.parser()
  defparser position(%ParserState{status: :ok} = state) do
    %ParserState{state | results: [{state.line, state.column}]}
  end

  @doc """
  Emits the current offset.

  The offset is a zero-based integer which determines current position in the
  input string.
  """
  @spec offset(Combine.previous_parser()) :: Combine.parser()
  defparser offset(%ParserState{status: :ok} = state) do
    %ParserState{state | results: [Map.get(state, :offset, 0) + state.column | state.results]}
  end

  @doc """
  Initializes the parser state for token parsing.

  This is a hacky workaround that properly initializes parser to match the position
  of the first token. Combine always initializes the position to col 0, line 1.
  When parsing tokens, that's not always correct, since there might be some ignored
  whitespaces before the first token. This parser will properly initialize the
  position without consuming any token. It should be invoked only at the beginning of
  the parse tree.
  """
  @spec init_token_parser(Combine.previous_parser()) :: Combine.parser()
  defparser init_token_parser(%ParserState{status: :ok} = state) do
    case state.input do
      [%Token{} = first_token | _] ->
        %{state | line: first_token.line, column: first_token.column}

      _ ->
        state
    end
  end

  @doc "Consumes a token of the given category."
  @spec token(Combine.previous_parser(), any) :: Combine.parser()
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

        %{
          state
          | line: next_line,
            column: next_column,
            input: next_tokens,
            results: [token | results]
        }

      [%Token{} = token | _] ->
        %{
          state
          | status: :error,
            error: "Unexpected token `#{to_string(token)}` at line #{state.line}, column #{state.column}}"
        }

      [other | _] ->
        raise "Input contained non-token #{inspect(other)}"
    end
  end

  @doc "Consumes any token."
  @spec any_token(Combine.previous_parser()) :: Combine.parser()
  defparser any_token(%ParserState{status: :ok, input: input, results: results} = state) do
    case input do
      [] ->
        %ParserState{
          status: :error,
          error: "Unexpected `eof` at line #{state.line}, column #{state.column + 1}."
        }

      [%Token{} = token | next_tokens] ->
        {next_line, next_column} =
          case next_tokens do
            [next_token | _] -> {next_token.line, next_token.column}
            [] -> {token.line, token.column}
          end

        %{
          state
          | line: next_line,
            column: next_column,
            input: next_tokens,
            results: [token | results]
        }
    end
  end

  @doc """
  Emits the token offset in the input string.

  The token offset is a zero-based integer which determines token position in the
  input string.
  """
  @spec token_offset(Combine.previous_parser()) :: Combine.parser()
  defparser token_offset(%ParserState{status: :ok, input: input, results: results} = state) do
    case input do
      [] ->
        %ParserState{
          status: :error,
          error: "Unexpected `eof` at line #{state.line}, column #{state.column + 1}."
        }

      [%Token{} = token | _] ->
        %{state | results: [token.offset | results]}
    end
  end

  @doc """
  Runs the parser and sets the error message if it fails, appending the position
  information.

  This is more flexible than `label`, because it allows you to set an arbitrary
  error message, whereas `label` can only used for `Expected ... at ...` messages.
  """
  @spec error_message(Combine.previous_parser(), Combine.parser(), String.t()) :: Combine.parser()
  defparser error_message(%ParserState{status: :ok} = state, parser, message) do
    next_state = parser.(state)
    with %ParserState{status: :error} <- next_state do
      %ParserState{
        next_state
        | error: "#{message} at line #{next_state.line}, column #{next_state.column + 1}."
      }
    end
  end

  @doc "Creates a parser that consumes no input and always returns the given value."
  @spec return(Combine.previous_parser(), any) :: Combine.parser()
  defparser return(%ParserState{status: :ok} = state, value) do
    %{state | results: [value | state.results]}
  end

  @doc """
  Creates a lazy parser.

  A lazy parser is created on demand. This can be useful to parse recursive grammars.

  For example, the following spec for parsing recursive parentheses won't work:

  ```
  defp parens do
    sequence([char("("), many(parens()), char(")")])
  end
  ```

  The reason is that parsers are by default eager, so invoking `parens()` will
  cause an infinite loop.

  Using `lazy`, you can defer recursing:

  ```
  defp parens do
    sequence([char("("), many(lazy(fn -> parens() end)), char(")")])
  end
  ```

  Lazy parser will be created on demand. In this example, if the next character is
  `(`, we'll recurse once. Then, if the next character is again `(`, we'll recurse
  again. Otherwise, we'll try to match the `)` character.
  """
  @spec lazy(Combine.previous_parser(), (() -> Combine.parser())) :: Combine.parser()
  defparser lazy(%ParserState{status: :ok} = state, generator) do
    generator.().(state)
  end

  @doc """
  Parses one or more instances of `term` separated by `separator`. Unlike the combine `sep_by1` consumes separators
  eagerly, making it possible to detect wrong `term`s that are later in the chain. For example for input of `"a,b,a"`
  `sep_by1(char("a"), char(","))` will consume only the first `a` and succeed while this one will fail on the `b`.

  Another significant difference from `sep_by1` is that this function returns the separators as part of the output, so
  parsing "a,a,a" with `sep_by1_eager(char("a"), char(","))` gives `["a", ",", "a", ",", "a"]`.
  """
  @spec sep_by1_eager(Combine.previous_parser(), Combine.parser(), Combine.parser()) :: Combine.parser()
  defparser sep_by1_eager(state, term, separator) do
    (Base.pair_both(
       term,
       switch([
         {separator, lazy(fn -> sep_by1_eager(term, separator) end)},
         {noop(), noop()}
       ])
     )
     |> Base.map(fn
       {first, {[sep], [rest]}} -> [first, sep | rest]
       {single, {[], []}} -> [single]
     end)).(state)
  end

  @doc """
  Parses zero or more instances of `term` separated by `separator` and terminated by `stop`. Consumes separators eagerly
  like `sep_by1_eager`. Having the `stop` be parsed by this parser allows better errors to be reported in case there is
  a single wrong term.
  """
  @spec sep_by_until(Combine.previous_parser(), Combine.parser(), Combine.parser(), Combine.parser()) ::
          Combine.parser()
  defparser sep_by_until(state, term, separator, stop) do
    (switch([
       {stop, return(:stop)},
       {Base.pair_both(term, separator), Base.pair_both(sep_by1_eager(term, separator), stop)},
       {Base.pair_both(term, stop), return(:single)}
     ])
     |> Base.map(fn
       {_, [:stop]} -> []
       {[{first, sep}], [{rest, stop}]} -> [first, sep | rest]
       {[{single, stop}], [:single]} -> [single]
     end)).(state)
  end

  @doc "Same as `choice_deepest_error([parser1, parser2])`"
  @spec either_deepest_error(Combine.previous_parser(), Combine.parser(), Combine.parser()) :: Combine.parser()
  defparser(
    either_deepest_error(state, parser1, parser2),
    do: choice_deepest_error([parser1, parser2]).(state)
  )

  @doc """
  Tries parsers in order and returns the result of the first one that succeeds. In case of failure returns the
  error returned by the parser which consumed the most input.
  """
  @spec choice_deepest_error(Combine.previous_parser(), [Combine.parser()]) :: Combine.parser()
  defparser(
    choice_deepest_error(%ParserState{status: :ok, line: line, column: column} = state, parsers),
    do:
      do_choice_deepest_error(
        parsers,
        state,
        "No possible parsers at line #{line}, column #{column}."
      )
  )

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp do_choice_deepest_error([], state, deepest_error), do: %{state | :status => :error, :error => deepest_error}

  defp do_choice_deepest_error([parser | rest], state, deepest_error) do
    case parser.(state) do
      %ParserState{status: :ok} = result ->
        result

      %ParserState{error: error} ->
        do_choice_deepest_error(rest, state, deeper_error(deepest_error, error))
    end
  end

  defp deeper_error(error1, error2), do: if(error_deepness(error1) > error_deepness(error2), do: error1, else: error2)

  @error_regex ~r/at line ([0-9]+), column ([0-9]+)/
  defp error_deepness(error) do
    case Regex.run(@error_regex, error) do
      [_, line, column] -> {String.to_integer(line), String.to_integer(column)}
      nil -> nil
    end
  end
end
