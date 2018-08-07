defmodule Air.Service.LDAP.FilterParser do
  @moduledoc """
  This module contains a parser for LDAP filter expressions. The takes in a string representation of the filter and
  produces data structures compatible with :eldap as its output. See https://ldap.com/ldap-filters/ for more on these
  filters.
  """

  import Combine.{Helpers, Parsers.Base, Parsers.Text}

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Parses the string representation of an LDAP filter into an :eldap.filter()."
  @spec parse(String.t()) :: {:ok, :eldap.filter()} | :error
  def parse(filter) do
    case Combine.parse(filter, parser()) do
      {:error, _} -> :error
      [result] -> {:ok, result}
    end
  end

  # -------------------------------------------------------------------
  # Parsers
  # -------------------------------------------------------------------

  defp parser(), do: filter() |> eof()

  defp filter() do
    lazy(fn ->
      choice([
        and_filter(),
        or_filter(),
        not_filter(),
        extensible_match(),
        presence_filter(),
        equality_filter(),
        gteq_filter(),
        lteq_filter(),
        approximate_match()
      ])
    end)
  end

  defp extensible_match() do
    sequence([
      char(?(),
      option(field()),
      option(string(":dn")),
      option(sequence([char(?:), field()])),
      string(":="),
      value(),
      char(?))
    ])
    |> map(fn [_, field, dn?, matching_rule, _, value, _] ->
      :eldap.extensibleMatch(value, extensible_match_options(field, dn?, matching_rule))
    end)
  end

  defp extensible_match_options(nil, dn?, rule), do: extensible_match_options(dn?, rule)
  defp extensible_match_options(value, dn?, rule), do: [{:type, value} | extensible_match_options(dn?, rule)]
  defp extensible_match_options(nil, rule), do: extensible_match_options(rule)
  defp extensible_match_options(_, rule), do: [{:dnAttributes, true} | extensible_match_options(rule)]
  defp extensible_match_options(nil), do: []
  defp extensible_match_options([_, rule]), do: [matchingRule: rule]

  defp presence_filter() do
    sequence([char(?(), field(), char(?=), char(?*), char(?))])
    |> map(fn [_, field, _, _, _] -> :eldap.present(field) end)
  end

  defp equality_filter() do
    sequence([char(?(), field(), char(?=), raw_value(), char(?))])
    |> map(fn [_, field, _, value, _] ->
      if String.contains?(value, "*") do
        options =
          value
          |> String.split("*")
          |> Enum.map(&unescape/1)
          |> Enum.map(&to_charlist/1)
          |> substring_options(:initial)

        :eldap.substrings(field, options)
      else
        :eldap.equalityMatch(field, value |> unescape() |> to_charlist())
      end
    end)
  end

  defp substring_options(['' | rest], :initial), do: substring_options(rest)
  defp substring_options([string | rest], :initial), do: [{:initial, string} | substring_options(rest)]
  defp substring_options(['']), do: []
  defp substring_options([final]), do: [{:final, final}]
  defp substring_options(['' | rest]), do: substring_options(rest)
  defp substring_options([string | rest]), do: [{:any, string} | substring_options(rest)]

  defp gteq_filter() do
    sequence([char(?(), field(), char(?>), char(?=), value(), char(?))])
    |> map(fn [_, field, _, _, value, _] -> :eldap.greaterOrEqual(field, value) end)
  end

  defp lteq_filter() do
    sequence([char(?(), field(), char(?<), char(?=), value(), char(?))])
    |> map(fn [_, field, _, _, value, _] -> :eldap.lessOrEqual(field, value) end)
  end

  defp approximate_match() do
    sequence([char(?(), field(), char(?~), char(?=), value(), char(?))])
    |> map(fn [_, field, _, _, value, _] -> :eldap.approxMatch(field, value) end)
  end

  defp and_filter() do
    sequence([char(?(), char(?&), many1(filter()), char(?))])
    |> map(fn [_, _, filters, _] -> :eldap.and(filters) end)
  end

  defp or_filter() do
    sequence([char(?(), char(?|), many1(filter()), char(?))])
    |> map(fn [_, _, filters, _] -> :eldap.or(filters) end)
  end

  defp not_filter() do
    sequence([char(?(), char(?!), filter(), char(?))])
    |> map(fn [_, _, filter, _] -> :eldap.not(filter) end)
  end

  defp field(), do: word_of(~r/[^=><~:]/) |> map(fn field -> to_charlist(field) end)

  defp value(), do: raw_value() |> map(fn value -> value |> unescape() |> to_charlist() end)

  defp raw_value(), do: word_of(~r/[^)]/)

  # -------------------------------------------------------------------
  # Helpers
  # -------------------------------------------------------------------

  defp unescape(value) do
    value
    |> String.replace("\00", "\0")
    |> String.replace("\28", "(")
    |> String.replace("\29", ")")
    |> String.replace("\2a", "*")
    |> String.replace("\5c", "\\")
  end

  defparser lazy(%Combine.ParserState{status: :ok} = state, generator) do
    generator.().(state)
  end
end
