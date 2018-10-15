defmodule Cloak.Sql.LikePattern do
  @moduledoc "Handles operations on like patterns."

  @opaque t :: {String.t(), String.t()}
  @type grapheme :: String.t() | :% | :_

  @special_characters [:%, :_]
  @standard_escape_character "\\"

  use Combine
  alias Cloak.Sql.Expression

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Returns a new opaque and normalized like pattern construct that can be used with the like pattern module."
  @spec new(String.t(), String.t() | nil) :: t
  def new(pattern, escape),
    do: {
      {pattern, escape}
      |> graphemes()
      |> do_normalize()
      |> Enum.map(&standard_escape/1)
      |> Enum.join(),
      @standard_escape_character
    }

  @doc """
  Returns a parsed representation of this like pattern. Regular characters are represented as one-character, while the
  special characters % and _ are represented as :% and :_.
  """
  @spec graphemes({String.t(), String.t() | nil}) :: [grapheme]
  def graphemes({pattern, escape}) do
    [result] = Combine.parse(pattern, parser(escape))
    result
  end

  @doc "Returns true if the pattern does not contain any special characters, false otherwise."
  @spec trivial?(t) :: boolean
  def trivial?(pattern), do: pattern |> graphemes() |> Enum.all?(&(not wildcard?(&1)))

  @doc "Returns true if the pattern is of the form 'foo', '%foo', 'foo%', or '%foo%', false otherwise."
  @spec simple?(t) :: boolean
  def simple?(pattern) do
    graphemes = graphemes(pattern)

    if Enum.any?(graphemes, &(&1 == :_)) do
      false
    else
      graphemes |> Enum.drop(1) |> Enum.take(length(graphemes) - 2) |> Enum.any?(&wildcard?/1) |> :erlang.not()
    end
  end

  @doc "Converts a constant like pattern expression into a text expression. Fails if the pattern is not `trivial?/1`."
  @spec trivial_to_string(Expression.t()) :: Expression.t()
  def trivial_to_string(expression) do
    true = trivial?(expression.value)
    Expression.constant(:text, expression.value |> graphemes() |> Enum.join())
  end

  @doc "Returns a regex pattern implementing the given like pattern."
  @spec to_regex_pattern(t) :: String.t()
  def to_regex_pattern(pattern),
    do:
      pattern
      |> graphemes()
      |> Enum.map(&to_regex_part/1)
      |> Enum.join()
      |> anchor()

  @doc "Returns a regex implementing the given pattern."
  @spec to_regex(t) :: Regex.t()
  def to_regex(pattern), do: pattern |> to_regex_pattern() |> Regex.compile!("usm")

  @doc "Returns a regex implementing the given pattern that matches case-insensitively."
  @spec to_case_insensitive_regex(t) :: Regex.t()
  def to_case_insensitive_regex(pattern), do: pattern |> to_regex_pattern() |> Regex.compile!("usmi")

  @doc "Lowercases the LIKE match pattern."
  @spec lowercase(t) :: t
  def lowercase({pattern, escape}), do: {String.downcase(pattern), escape}

  # -------------------------------------------------------------------
  # Parsing
  # -------------------------------------------------------------------

  defp parser(escape),
    do:
      many(
        choice([
          escaped_character(escape),
          special_character(),
          char()
        ])
      )

  defp escaped_character(nil), do: fail("no escape specified")
  defp escaped_character(escape), do: pipe([char(escape), char()], fn [^escape, c] -> c end)

  defp special_character(),
    do:
      either(char("%"), char("_"))
      |> map(&String.to_existing_atom/1)

  # -------------------------------------------------------------------
  # Normalization
  # -------------------------------------------------------------------

  defp do_normalize(graphemes),
    do:
      graphemes
      |> Enum.chunk_by(&wildcard?/1)
      |> Enum.flat_map(&normalize_chunk/1)

  defp normalize_chunk(chunk = [first | _]) when first == :_ or first == :% do
    percent = if Enum.member?(chunk, :%), do: :%, else: ""
    rest = Enum.filter(chunk, &(&1 == :_))
    [percent | rest]
  end

  defp normalize_chunk(chunk), do: chunk

  defp standard_escape(@standard_escape_character), do: @standard_escape_character <> @standard_escape_character

  defp standard_escape("%"), do: "#{@standard_escape_character}%"
  defp standard_escape("_"), do: "#{@standard_escape_character}_"
  defp standard_escape(char), do: char

  # -------------------------------------------------------------------
  # Helpers
  # -------------------------------------------------------------------

  defp wildcard?(string), do: string in @special_characters

  defp to_regex_part(:%), do: ".*"
  defp to_regex_part(:_), do: "."
  defp to_regex_part(char), do: Regex.escape(char)

  defp anchor(pattern), do: "^#{pattern}$"
end
