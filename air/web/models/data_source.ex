defmodule Air.DataSource do
  @moduledoc "Represents data sources made available through the cloaks"
  use Air.Web, :model

  alias Air.{DataSource, Group, Repo}

  @type t :: %__MODULE__{}

  schema "data_sources" do
    field :global_id, :string
    field :name, :string
    field :tables, :string

    has_many :queries, Air.Query
    many_to_many :groups, Group,
      join_through: "data_sources_groups",
      on_delete: :delete_all,
      on_replace: :delete

    timestamps
  end

  @required_fields ~w(name tables global_id)a
  @optional_fields ~w()a


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Returns a map representation of the data source tables"
  @spec tables(DataSource.t) :: [Map.t]
  def tables(data_source) do
    case Poison.decode(data_source.tables) do
      {:ok, tables} -> tables
      _ -> []
    end
  end

  @doc "Format a data source as a map"
  @spec to_map(DataSource.t) :: Map.t
  def to_map(data_source) do
    %{
      id: data_source.id,
      token: data_source.global_id,
      name: data_source.name,
      tables: tables(data_source),
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
    |> PhoenixMTM.Changeset.cast_collection(:groups, Air.Repo, Group)
  end

  @doc "Returns a boolean whether a data source is available to a user"
  @spec available_to_user?(integer, User.t) :: boolean
  def available_to_user?(data_source_id, user) do
    Repo.get(for_user(user), data_source_id) !== nil
  end


  # -------------------------------------------------------------------
  # Query functions
  # -------------------------------------------------------------------

  @doc "Adds a query filter selecting only those for the given user"
  @spec for_user(User.t) :: Ecto.Queryable.t
  def for_user(user) do
    from data_source in __MODULE__,
    inner_join: group in assoc(data_source, :groups),
    inner_join: user in assoc(group, :users),
    where: user.id == ^user.id
  end
end
