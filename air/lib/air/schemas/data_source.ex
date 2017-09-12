defmodule Air.Schemas.DataSource do
  @moduledoc "Represents data sources made available through the cloaks"
  use Air.Schemas.Base

  alias Air.Schemas.{Group, Query}

  @type t :: %__MODULE__{}

  schema "data_sources" do
    field :name, :string
    field :description, :string
    field :tables, :string
    field :errors, :string

    has_many :queries, Query,
      on_delete: :delete_all
    many_to_many :groups, Group,
      join_through: "data_sources_groups",
      on_delete: :delete_all,
      on_replace: :delete

    timestamps()
  end


  # --------------------------------------------------------------------
  # API functions
  # --------------------------------------------------------------------

  @doc "Returns a list of the data source errors"
  @spec errors(t) :: [String.t]
  def errors(data_source) do
    case Poison.decode(data_source.errors) do
      {:ok, errors} -> errors
      _ -> []
    end
  end

  @doc "Returns a map representation of the data source tables"
  @spec tables(t) :: [Map.t]
  def tables(data_source) do
    case Poison.decode(data_source.tables) do
      {:ok, tables} -> tables
      _ -> []
    end
  end
end
