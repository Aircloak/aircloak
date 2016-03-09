defmodule Air.User do
  @moduledoc "The user model"
  require Logger

  use Air.Web, :model

  alias Ecto.Changeset
  alias Comeonin.Pbkdf2, as: Hash
  alias Air.Organisation

  @type t :: %__MODULE__{}

  schema "users" do
    field :email, :string
    field :hashed_password, :string
    field :name, :string

    belongs_to :organisation, Organisation

    timestamps

    # These virtual fields are used for validation,
    # but never persisted to the database
    field :password, :string, virtual: true
    field :password_confirmation, :string, virtual: true
  end

  @required_fields ~w(email name organisation_id)
  @optional_fields ~w(password password_confirmation)

  @doc """
  Creates a changeset based on the `model` and `params`.

  If no params are provided, an invalid changeset is returned
  with no validation performed.
  """
  @spec changeset(t, %{binary => term} | %{atom => term} | :empty) :: Changeset.t
  def changeset(model, params \\ :empty) do
    model
    |> cast(params, @required_fields, @optional_fields)
    |> validate_format(:email, ~r/@/)
    |> validate_length(:name, min: 2)
    |> validate_confirmation(:password)
    |> possibly_update_password_hash
    |> unique_constraint(:email)
  end

  defp possibly_update_password_hash(%Changeset{valid?: true, changes: %{password: password}} = changeset)
      when password != "" do
    put_change(changeset, :hashed_password, Hash.hashpwsalt(password))
  end
  defp possibly_update_password_hash(changeset), do: changeset

  @doc "Validates the user password"
  @spec validate_password(nil | t, String.t) :: boolean
  def validate_password(nil, _password), do: Hash.dummy_checkpw
  def validate_password(user, password) do
    Hash.checkpw(password, user.hashed_password)
  end
end
