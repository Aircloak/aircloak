defmodule Air.DataSource do
  @moduledoc "Represents data sources made available through the cloaks"
  use Air.Web, :model

  alias Air.{Cloak, DataSource, Repo}

  @type t :: %__MODULE__{}

  schema "data_sources" do
    field :name, :string
    field :tables, :string

    belongs_to :cloak, Cloak

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


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp changeset(model, params) do
    model
    |> cast(params, @required_fields, @optional_fields)
  end
end
