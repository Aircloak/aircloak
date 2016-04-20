defmodule Air.CloakInfo do
  @moduledoc """
  Cloak model.

  This module defines a struct which contains data passed by the Cloak, such as
  cloak name, organisation, and data sources.
  """
  defstruct [:name, :organisation, :data_sources]

  @type t :: %__MODULE__{
    name: String.t,
    organisation: String.t,
    data_sources: [data_source]
  }
  @type data_source :: %{id: String.t, tables: [table]}
  @type table :: %{id: String.t, columns: [column]}
  @type column :: %{name: String.t, type: String.t}


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Creates the new structure from raw data passed by the cloak."
  @spec new(%{String.t => any}) :: t
  def new(cloak_info) do
    %__MODULE__{
      name: Map.fetch!(cloak_info, "name"),
      organisation: "unknown_org", # TODO: fix this when there's org info
      data_sources: Map.fetch!(cloak_info, "data_sources") |> parse_data_sources()
    }
  end


  # -------------------------------------------------------------------
  # Private functions
  # -------------------------------------------------------------------

  defp parse_data_sources(data_sources) do
    for data_source <- data_sources do
      %{
        id: Map.fetch!(data_source, "id"),
        tables: Map.fetch!(data_source, "tables") |> parse_tables()
      }
    end
  end

  defp parse_tables(tables) do
    for table <- tables do
      %{
        id: Map.fetch!(table, "id"),
        columns: Map.fetch!(table, "columns") |> parse_columns()
      }
    end
  end

  defp parse_columns(columns) do
    for column <- columns do
      %{
        name: Map.fetch!(column, "name"),
        type: Map.fetch!(column, "type")
      }
    end
  end
end
