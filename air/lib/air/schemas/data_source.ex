defmodule Air.Schemas.DataSource do
  @moduledoc "Represents data sources made available through the cloaks"
  use Air.Schemas.Base

  alias Air.Schemas.Group

  @type t :: %__MODULE__{}

  schema "data_sources" do
    field :global_id, :string
    field :name, :string
    field :description, :string
    field :tables, :string
    field :errors, :string

    has_many :queries, Air.Schemas.Query
    many_to_many :groups, Group,
      join_through: "data_sources_groups",
      on_delete: :delete_all,
      on_replace: :delete

    timestamps()
  end


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Returns a map representation of the data source tables"
  @spec tables(t) :: [Map.t]
  def tables(data_source) do
    case Poison.decode(data_source.tables) do
      {:ok, tables} -> tables
      _ -> []
    end
  end

  @doc "Returns a list of the data source errors"
  @spec errors(t) :: [String.t]
  def errors(data_source) do
    case Poison.decode(data_source.errors) do
      {:ok, errors} -> errors
      _ -> []
    end
  end

  @doc "Format a data source as a map"
  @spec to_map(t) :: Map.t
  def to_map(data_source) do
    %{
      id: data_source.id,
      token: data_source.global_id,
      name: data_source.name,
      description: data_source.description,
      tables: tables(data_source),
      errors: errors(data_source),
    }
  end
end
