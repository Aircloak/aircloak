defmodule Central.Schemas.User do
  @moduledoc "The user schema."
  require Logger

  use Central.Web, :model

  alias Ecto.Changeset
  alias Central.Schemas.User
  alias Comeonin.Pbkdf2, as: Hash

  @type t :: %__MODULE__{}

  schema "users" do
    field :email, :string
    field :hashed_password, :string
    field :name, :string

    timestamps()

    # These virtual fields are used for validation,
    # but never persisted to the database
    field :password, :string, virtual: true
    field :password_confirmation, :string, virtual: true
  end

  @required_fields ~w(email name)a
  @optional_fields ~w(password password_confirmation)a


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

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
    |> validate_format(:email, ~r/@/)
    |> validate_length(:name, min: 2)
    |> validate_length(:password, min: 4)
    |> validate_confirmation(:password)
    |> update_password_hash
    |> unique_constraint(:email)
  end

  @doc "Returns an empty changeset to use in forms"
  @spec empty_changeset() :: Changeset.t
  def empty_changeset() do
    User.changeset(%User{})
  end

  @doc """
  Relies on changeset/2 for validation and casting of parameters,
  but additionally ensures that a password has been provided as well.
  """
  @spec new_user_changeset(t | Changeset.t, Map.t) :: Changeset.t
  def new_user_changeset(model, params \\ %{}) do
    model
    |> changeset(params)
    |> validate_required([:password, :password_confirmation])
  end

  @doc "Validates the user password."
  @spec validate_password(nil | t, String.t) :: boolean
  def validate_password(nil, _password), do: Hash.dummy_checkpw
  def validate_password(user, password), do: Hash.checkpw(password, user.hashed_password)


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp update_password_hash(%Changeset{valid?: true, changes: %{password: password}} = changeset)
      when password != "" do
    put_change(changeset, :hashed_password, Hash.hashpwsalt(password))
  end
  defp update_password_hash(changeset), do: changeset

  defimpl Inspect do
    @moduledoc """
    Custom inspection of the user record, where we only present non-sensitive fields.

    This allows us to safely inspect user anywhere in log expressions without
    worrying we'll leek some sensitive data.
    """
    def inspect(user, opts) do
      Inspect.Map.inspect(Map.take(user, [:id]), inspect(user.__struct__), opts)
    end
  end
end
