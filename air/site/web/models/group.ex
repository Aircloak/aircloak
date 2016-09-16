defmodule Air.Group do
  @moduledoc """
  Groups allows the air system to manage authorization of data sources.
  A group can have many users and likewise give access to many distinct data sources.
  A user in turn can be a member of many groups, and a data source can be made
  available to users of any number of groups. There is therefore a many to many to many
  relationship between users, groups and data sources.
  """
  use Air.Web, :model

  alias Air.{User, DataSource}

  @type t :: %__MODULE__{}

  schema "groups" do
    field :name, :string
    many_to_many :users, User,
      join_through: "groups_users",
      on_delete: :delete_all,
      on_replace: :delete
    many_to_many :data_sources, DataSource,
      join_through: "data_sources_groups",
      on_delete: :delete_all,
      on_replace: :delete

    timestamps
  end

  @required_fields ~w(name)a
  @optional_fields ~w()a

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
    |> unique_constraint(:name)
  end
end
