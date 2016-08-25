defmodule Air.DataSource do
  @moduledoc "Represents data sources made available through the cloaks"
  use Air.Web, :model

  alias Air.{Cloak, DataSource, Repo, Query}

  @type t :: %__MODULE__{}

  schema "data_sources" do
    field :name, :string
    field :tables, :string

    belongs_to :cloak, Cloak
    has_many :queries, Query

    timestamps
  end

  @required_fields ~w(name tables cloak_id)
  @optional_fields ~w()


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc """
  Create, or updates, a data source entry for a cloak.
  The data source is associated with the cloak, and the
  table data encoded as JSON
  """
  @spec register(Cloak.t, Map.t) :: :ok
  def register(cloak, data) do
    name = data["id"]
    params = %{
      cloak_id: cloak.id,
      name: name,
      tables: Poison.encode!(data["tables"]),
    }

    case Repo.one(from ds in DataSource, where: ds.name == ^name and ds.cloak_id == ^cloak.id) do
      nil ->
        %DataSource{}
        |> changeset(params)
        |> Repo.insert!()
      data_source ->
        data_source
        |> changeset(params)
        |> Repo.update!
    end
    :ok
  end

  @doc "Returns a map representation of the data source tables"
  @spec tables(DataSource.t) :: [Map.t]
  def tables(data_source) do
    Poison.decode!(data_source.tables)
  end

  @doc "Format a data source as a map"
  @spec to_map(DataSource.t) :: Map.t
  def to_map(data_source) do
    %{
      id: data_source.id,
      # For backwards compatibility, we keep token in the structure,
      # so old API clients keep on working
      token: data_source.id,
      name: data_source.name,
      tables: tables(data_source),
    }
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp changeset(model, params) do
    cast(model, params, @required_fields, @optional_fields)
  end
end
