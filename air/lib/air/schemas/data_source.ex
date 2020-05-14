defmodule Air.Schemas.DataSource do
  @moduledoc "Represents data sources made available through the cloaks"
  use Air.Schemas.Base

  alias Air.Schemas.{ExplorerAnalysis, Group, Query}

  @type t :: %__MODULE__{}

  schema "data_sources" do
    field(:name, :string)
    field(:description, :string)
    field(:tables, :string)
    field(:errors, :string)
    field(:database_host, :string)
    field(:columns_count, :integer)
    field(:isolated_computed_count, :integer)
    field(:shadow_tables_computed_count, :integer)
    field(:bounds_computed_count, :integer)
    field(:isolated_failed, {:array, :string})
    field(:shadow_tables_failed, {:array, :string})
    field(:bounds_failed, {:array, :string})
    field(:pending_delete, :boolean)
    field(:supports_analyst_tables, :boolean)

    has_many(:queries, Query, on_delete: :delete_all)
    has_many(:explorer_analyses, ExplorerAnalysis, on_delete: :delete_all)

    many_to_many(
      :groups,
      Group,
      join_through: "data_sources_groups",
      on_delete: :delete_all,
      on_replace: :delete
    )

    timestamps(type: :naive_datetime_usec)
  end

  # --------------------------------------------------------------------
  # API functions
  # --------------------------------------------------------------------

  @doc "Returns a list of the data source errors"
  @spec errors(t) :: [String.t()]
  def errors(data_source) do
    case Jason.decode(data_source.errors) do
      {:ok, errors} -> errors
      _ -> []
    end
  end

  @doc "Returns a map representation of the data source tables"
  @spec tables(t) :: [Map.t()]
  def tables(data_source) do
    case Jason.decode(data_source.tables) do
      {:ok, tables} -> tables
      _ -> []
    end
  end

  @doc "Returns true if the datasource has been analyzed, false otherwise."
  @spec analyzed?(t) :: boolean
  def analyzed?(data_source) do
    analyzed_isolated_columns = Enum.count(data_source.isolated_failed) + data_source.isolated_computed_count
    analyzed_shadow_columns = Enum.count(data_source.shadow_tables_failed) + data_source.shadow_tables_computed_count
    analyzed_bounds = Enum.count(data_source.bounds_failed) + data_source.bounds_computed_count

    analyzed_isolated_columns == data_source.columns_count and analyzed_shadow_columns == data_source.columns_count and
      analyzed_bounds == data_source.columns_count
  end
end
