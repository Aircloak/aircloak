defmodule Air.User do
  require Logger

  use Air.Web, :model

  alias Ecto.Changeset
  alias Comeonin.Pbkdf2, as: Hash

  schema "users" do
    field :email, :string
    field :hashed_password, :string
    field :name, :string

    timestamps

    # These virtual fields are used for validation,
    # but never persisted to the database
    field :password, :string, virtual: true
    field :password_confirmation, :string, virtual: true
  end

  @required_fields ~w(email name)
  @optional_fields ~w(password password_confirmation)

  @doc """
  Creates a changeset based on the `model` and `params`.

  If no params are provided, an invalid changeset is returned
  with no validation performed.
  """
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

  def validate_password(nil, password), do: Hash.dummy_checkpw
  def validate_password(user, password) do
    Hash.checkpw(password, user.hashed_password)
  end
end
