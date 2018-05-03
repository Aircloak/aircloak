defmodule Cloak.Query.Result do
  @moduledoc """
  Module for defining a query result struct that in addition to query results
  allows a result to contain meta-data about the query execution.
  """

  alias Cloak.Sql.Query

  @type t :: %__MODULE__{
          buckets: [bucket],
          columns: [String.t()],
          types: [atom],
          features: map
        }

  @type bucket :: %{row: [Cloak.DataSource.field()], occurrences: pos_integer}

  defstruct buckets: [], columns: [], types: [], features: %{}

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Creates the result struct that corresponds to the given query."
  @spec new([bucket], [String.t()], Query.features()) :: t
  def new(buckets, columns, features),
    do: %__MODULE__{
      buckets: normalize(buckets),
      columns: columns,
      features: features
    }

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp normalize(buckets), do: Enum.map(buckets, &%{&1 | row: normalize_for_encoding(&1.row)})

  defp normalize_for_encoding(row),
    # We're normalizing some Elixir structs, so they can be encoded to non-Elixir formats, such as JSON.
    do:
      Enum.map(row, fn
        %Date{} = date ->
          Date.to_iso8601(date)

        %Time{} = time ->
          Time.to_iso8601(time)

        %NaiveDateTime{} = naive_date_time ->
          NaiveDateTime.to_iso8601(naive_date_time)

        %Timex.Duration{} = duration ->
          Timex.Duration.to_string(duration)

        other ->
          other
      end)
end
