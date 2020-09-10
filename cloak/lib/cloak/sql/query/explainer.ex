defmodule Cloak.Sql.Query.Explainer do
  @moduledoc "Shows the classification of queries and their subqueries."

  alias Cloak.Sql.Query

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Returns a map containing information about the given query and its subqueries."
  @spec explain(Query.t()) :: map
  def explain(query),
    do: %{
      anonymization_type: query.anonymization_type,
      emulated?: query.emulated?,
      query_type: query.type,
      view?: query.view?,
      noise_layers: query.noise_layers,
      subqueries: explain_from(query, query.from)
    }

  @doc "Pretty-prints the query explanation to a list of rows."
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
        format_properties(explanation, alias),
        ")"
      ])

    [query | Enum.flat_map(explanation.subqueries, &format_from(&1, depth + 1))]
  end

  defp format_alias("__ac_" <> name), do: name
  defp format_alias(name), do: name

  defp format_from({:subquery, %{query: query, alias: alias}}, depth),
    do: format_explanation(query, alias, depth)

  defp format_from({:table, %{alias: alias, content_type: content_type}}, depth),
    do: ["#{indent(depth)}--> #{alias} (#{format_content_type(content_type)} table)"]

  defp format_content_type(:private), do: "personal"
  defp format_content_type(:public), do: "non-personal"

  defp format_properties(explanation, alias) do
    [
      if(String.starts_with?(alias, "__ac_"), do: "Aircloak generated"),
      if(explanation.view?, do: "view"),
      if(explanation.emulated?, do: "emulated"),
      explanation.query_type
      | if(explanation.query_type == :anonymized,
          do: [
            explanation.anonymization_type,
            case explanation.noise_layers do
              [] -> "default noise layer"
              [_noise_layer] -> "1 noise layer"
              noise_layers -> "#{Enum.count(noise_layers)} noise layers"
            end
          ],
          else: []
        )
    ]
    |> Enum.reject(&is_nil/1)
    |> Enum.join(", ")
  end

  defp explain_from(_, nil), do: []

  defp explain_from(query, table) when is_binary(table) do
    {:ok, %{content_type: content_type}} = Query.resolve_table(query, table)
    [{:table, %{alias: table, content_type: content_type}}]
  end

  defp explain_from(_, {:subquery, %{ast: ast, alias: alias}}),
    do: [{:subquery, %{alias: alias, query: explain(ast)}}]

  defp explain_from(query, {:join, %{lhs: lhs, rhs: rhs}}),
    do: explain_from(query, lhs) ++ explain_from(query, rhs)

  @indent_size 2
  defp indent(depth), do: String.duplicate(" ", depth * @indent_size)
end
