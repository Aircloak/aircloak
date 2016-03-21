defmodule Air.Organisation do
  @moduledoc "The organisation model"
  use Air.Web, :model

  alias Air.User

  @type t :: %__MODULE__{}

  schema "organisations" do
    field :name, :string

    has_many :users, User

    timestamps
  end

  @required_fields ~w(name)
  @optional_fields ~w()

  @doc """
  Creates a changeset based on the `model` and `params`.

  If no params are provided, an invalid changeset is returned
  with no validation performed.
  """
  @spec changeset(t, %{binary => term} | %{atom => term} | :empty) :: Ecto.Changeset.t
  def changeset(model, params \\ :empty) do
    model
    |> cast(params, @required_fields, @optional_fields)
    |> unique_constraint(:name)
  end
end
