defmodule Cloak.Sql.LikePattern do
  @type t :: {String.t, String.t}

  alias Cloak.Sql.Expression

  def graphemes({pattern, _escape}), do: String.graphemes(pattern)

  def trivial?(pattern), do:
    pattern |> graphemes() |> Enum.all?(& not special_like_char?(&1))

  def trivial_to_string(expression) do
    true = trivial?(expression.value)
    {pattern, _escape} = expression.value
    Expression.constant(:text, pattern)
  end

  defp special_like_char?(string), do: string == "_" or string == "%"
end
