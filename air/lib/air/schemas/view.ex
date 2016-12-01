defmodule Air.Schemas.View do
  @moduledoc "Schema for user-defined views"
  use Air.Schemas.Base
  require Logger

  @type t :: %__MODULE__{}

  schema "views" do
    field :name, :string
    field :sql, :string

    belongs_to :user, Air.Schemas.User
    belongs_to :data_source, Air.Schemas.User
  end

  @required_fields ~w(name sql)a
  @optional_fields ~w()a

  @doc """
  Creates a changeset based on the `schema` and `params`.

  If no params are provided, an invalid changeset is returned
  with no validation performed.
  """
  @spec changeset(t | Changeset.t, Map.t) :: Changeset.t
  def changeset(schema, params \\ %{}) do
    schema
    |> cast(params, @required_fields ++ @optional_fields)
    |> validate_required(@required_fields)
  end
end
