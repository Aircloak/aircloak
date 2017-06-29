defmodule Cloak.Sql.LikePattern do
  @type t :: {String.t, String.t}

  @special_characters [:%, :_]
  @standard_escape_character "\\"

  use Combine
  alias Cloak.Sql.Expression

  def graphemes({pattern, escape}) do
    [result] = Combine.parse(pattern, parser(escape))
    result
  end

  def trivial?(pattern), do:
    pattern |> graphemes() |> Enum.all?(& not special_like_char?(&1))

  def trivial_to_string(expression) do
    true = trivial?(expression.value)
    Expression.constant(:text, expression.value |> graphemes() |> Enum.join())
  end

  def normalize(pattern), do:
    {
      pattern
      |> graphemes()
      |> do_normalize()
      |> Enum.map(&standard_escape/1)
      |> Enum.join(),
      @standard_escape_character
    }

  def to_regex(pattern, options \\ ""), do:
    pattern
    |> graphemes()
    |> Enum.map(&to_regex_part/1)
    |> Enum.join()
    |> anchor()
    |> Regex.compile!(options)

  defp special_like_char?(string), do: string in @special_characters

  defp parser(escape), do:
    many(choice([
      escaped_character(escape),
      special_character(),
      char()
    ]))

  defp escaped_character(nil), do: fail("no escape specified")
  defp escaped_character(escape), do:
    pipe([char(escape), char()], fn([^escape, c]) -> c end)

  defp special_character(), do:
    either(char("%"), char("_"))
    |> map(&String.to_existing_atom/1)

  defp standard_escape(@standard_escape_character), do: @standard_escape_character <> @standard_escape_character
  defp standard_escape("%"), do: "#{@standard_escape_character}%"
  defp standard_escape("_"), do: "#{@standard_escape_character}_"
  defp standard_escape(char), do: char

  defp do_normalize(graphemes), do:
    graphemes
    |> Enum.chunk_by(&special_char?/1)
    |> Enum.flat_map(&normalize_chunk/1)

  defp normalize_chunk(chunk = [first | _]) when first == :_ or first == :% do
    percent = if Enum.member?(chunk, :%), do: :%, else: ""
    rest = Enum.filter(chunk, &(&1 == :_))
    [percent | rest]
  end
  defp normalize_chunk(chunk), do: chunk

  defp special_char?(string), do: string == :% or string == :_

  defp to_regex_part(:%), do: ".*"
  defp to_regex_part(:_), do: "."
  defp to_regex_part(char), do: Regex.escape(char)

  defp anchor(pattern), do: "^#{pattern}$"
end
