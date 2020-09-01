defmodule Cloak.Sql.Query.Explainer do
  @moduledoc "Shows the classification of queries and their subqueries."

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @spec explain(Cloak.Sql.Query.t()) :: map
  def explain(query),
    do: %{
      anonymization_type: query.anonymization_type,
      emulated?: query.emulated?,
      query_type: query.type,
      view?: query.view?,
      noise_layers: query.noise_layers,
      subqueries: explain_from(query.from)
    }

  @spec format_explanation(map) :: [String.t()]
  def format_explanation(explanation), do: format_explanation(explanation, "query", 0)

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp format_explanation(explanation, alias, depth) do
    query =
      Enum.join([
        indent(depth),
        if(depth > 0, do: "--> ", else: ""),
        format_alias(alias),
        " (",
        format_properties(explanation),
        ")"
      ])

    [query] ++ Enum.flat_map(explanation.subqueries, &format_from(&1, depth + 1))
  end

  defp format_alias("__ac_" <> name), do: "Aircloak:#{name}"
  defp format_alias(name), do: name

  defp format_from({:subquery, %{query: query, alias: alias}}, depth),
    do: format_explanation(query, alias, depth)

  defp format_from({:table, table}, depth),
    do: ["#{indent(depth)}--> #{table} (table)"]

  defp format_properties(explanation) do
    [
      if(explanation.view?, do: "view"),
      if(explanation.emulated?, do: "emulated"),
      Atom.to_string(explanation.query_type),
      if(explanation.query_type == :anonymized, do: Atom.to_string(explanation.anonymization_type)),
      case explanation.noise_layers do
        [] -> nil
        [_noise_layer] -> "1 noise layer"
        noise_layers -> "#{Enum.count(noise_layers)} noise layers"
      end
    ]
    |> Enum.reject(&is_nil/1)
    |> Enum.join(", ")
  end

  defp explain_from(nil), do: []

  defp explain_from(table) when is_binary(table),
    do: [{:table, table}]

  defp explain_from({:subquery, %{ast: ast, alias: alias}}),
    do: [{:subquery, %{alias: alias, query: explain(ast)}}]

  defp explain_from({:join, %{lhs: lhs, rhs: rhs}}),
    do: explain_from(lhs) ++ explain_from(rhs)

  @indent_size 2
  defp indent(depth), do: String.duplicate(" ", depth * @indent_size)
end
