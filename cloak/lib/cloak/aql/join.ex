defmodule Cloak.Aql.Join do
  @moduledoc "Helpers for working with joins"

  @doc "Matches a cross join"
  defmacro cross_join(lhs, rhs) do
    quote do
      {:join, :cross_join, unquote(lhs), unquote(rhs)}
    end
  end

  for join_type <- [:inner_join, :full_outer_join, :left_outer_join, :right_outer_join] do
    join_human_friendly_name =
      join_type
      |> to_string()
      |> String.replace("_", " ")

    @doc "Matches a #{join_human_friendly_name}"
    defmacro unquote(join_type)(lhs, rhs, on) do
      join_type = unquote(join_type)
      quote do
        join_with_condition(unquote(join_type), unquote(lhs), unquote(rhs), unquote(on))
      end
    end
  end

  @doc "Matches a join with an `ON` condition provided"
  defmacro join_with_condition(join_type, lhs, rhs, on) do
    quote do
      {:join, unquote(join_type), unquote(lhs), unquote(rhs), :on, unquote(on)}
    end
  end
end
