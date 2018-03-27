defmodule Central.Schemas.Customer do
  @moduledoc "The customer schema."

  use Central.Web, :model

  alias Ecto.Changeset
  alias Central.Schemas.{License, Query}

  @type t :: %__MODULE__{}

  schema "customers" do
    field(:name, :string)

    has_many(:queries, Query)
    has_many(:licenses, License)

    timestamps()
  end

  @required_fields ~w(name)a
  @optional_fields ~w()a

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc """
  Creates a changeset based on the `model` and `params`.

  If no params are provided, an invalid changeset is returned
  with no validation performed.
  """
  @spec changeset(t | Changeset.t(), Map.t()) :: Changeset.t()
  def changeset(model, params \\ %{}) do
    model
    |> cast(params, @required_fields ++ @optional_fields)
    |> unique_constraint(:name)
  end

  @doc "Validates a changeset for insertion"
  @spec new_customer_changeset(t | Changeset.t(), Map.t()) :: Changeset.t()
  def new_customer_changeset(model, params \\ %{}) do
    model
    |> changeset(params)
    |> validate_required(@required_fields)
    |> validate_length(:name, min: 2)
  end

  @doc "Returns an empty changeset to use in forms"
  @spec empty_changeset() :: Changeset.t()
  def empty_changeset() do
    changeset(%__MODULE__{})
  end
end
