defmodule Air.Task do
  @moduledoc "The task model."
  use Air.Web, :model

  alias Air.User

  @primary_key {:id, :binary_id, autogenerate: true}
  schema "tasks" do
    field :name, :string
    field :query, :string
    belongs_to :user, User

    timestamps
  end

  @required_fields ~w(name query)
  @optional_fields ~w()

  @doc """
  Creates a changeset based on the `model` and `params`.

  If no params are provided, an invalid changeset is returned
  with no validation performed.
  """
  def changeset(model, params \\ :empty) do
    model
    |> cast(params, @required_fields, @optional_fields)
    |> validate_length(:name, min: 1)
  end
end
