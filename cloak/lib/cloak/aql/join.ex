defmodule Cloak.Aql.Join do
  @moduledoc "Helpers for working with joins"

  alias Cloak.Aql.Parser

  @type cross_join :: {:join, Parser.from_clause, Parser.from_clause, []}
  @type join_with_conditions ::
    {
      :inner_join | :full_outer_join | :left_outer_join | :right_outer_join,
      Parser.from_clause, Parser.from_clause, [Parser.where_clause]
    }
  @type t :: cross_join | join_with_conditions

  @doc "Matches a cross join"
  defmacro cross_join(lhs, rhs) do
    quote do
      join(:cross_join, unquote(lhs), unquote(rhs), [])
    end
  end

  for join_type <- [:inner_join, :full_outer_join, :left_outer_join, :right_outer_join] do
    join_human_friendly_name =
      join_type
      |> to_string()
      |> String.replace("_", " ")

    @doc "Matches a #{join_human_friendly_name}"
    defmacro unquote(join_type)(lhs, rhs, conditions) do
      join_type = unquote(join_type)
      quote do
        join(unquote(join_type), unquote(lhs), unquote(rhs), unquote(conditions))
      end
    end
  end

  @doc "Matches a join"
  defmacro join(join_type, lhs, rhs, conditions) do
    quote do
      {:join, unquote(join_type), unquote(lhs), unquote(rhs), unquote(conditions)}
    end
  end
end
