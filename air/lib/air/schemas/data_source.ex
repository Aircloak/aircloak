defmodule Air.Schemas.DataSource do
  @moduledoc "Represents data sources made available through the cloaks"
  use Air.Schemas.Base

  alias Air.Schemas.Group
  alias Air.Service

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

  @required_fields ~w(name tables global_id)a
  @optional_fields ~w(errors description)a


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Format a data source as a map"
  @spec to_map(t) :: Map.t
  def to_map(data_source) do
    %{
      id: data_source.id,
      token: data_source.global_id,
      name: data_source.name,
      description: data_source.description,
      tables: Service.DataSource.tables(data_source),
      errors: Service.DataSource.errors(data_source),
    }
  end

  @doc """
  Creates a changeset based on the `model` and `params`.

  If no params are provided, an invalid changeset is returned
  with no validation performed.
  """
  @spec changeset(t | Changeset.t, Map.t) :: Changeset.t
  def changeset(model, params \\ %{}) do
    model
    |> cast(params, @required_fields ++ @optional_fields)
    |> validate_required(@required_fields)
    |> unique_constraint(:global_id)
    |> unique_constraint(:name)
    |> PhoenixMTM.Changeset.cast_collection(:groups, Air.Repo, Group)
  end
end
