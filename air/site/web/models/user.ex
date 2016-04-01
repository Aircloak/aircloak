defmodule Air.User do
  @moduledoc "The user model."
  require Logger

  use Air.Web, :model

  alias Ecto.Changeset
  alias Comeonin.Pbkdf2, as: Hash
  alias Air.Organisation

  @type t :: %__MODULE__{}
  @type role_id :: non_neg_integer
  @type role_key :: atom

  schema "users" do
    field :email, :string
    field :hashed_password, :string
    field :name, :string
    field :role_id, :integer

    belongs_to :organisation, Organisation

    timestamps

    # These virtual fields are used for validation,
    # but never persisted to the database
    field :password, :string, virtual: true
    field :password_confirmation, :string, virtual: true
  end

  @required_fields ~w(email name organisation_id role_id)
  @optional_fields ~w(password password_confirmation)

  @roles %{
    0 => {:user, "user"},
    1 => {:org_admin, "organisation manager"},
    2 => {:admin, "administrator"}
  }

  @included_roles %{
    user: [],
    org_admin: [:user],
    admin: [:org_admin, :user]
  }


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Returns a list of all supported roles."
  @spec all_roles :: %{role_id => {role_key, description::String.t}}
  def all_roles, do: @roles

  @doc "Returns all user's roles."
  @spec roles(t | Changeset.t) :: [role_key]
  def roles(user) do
    expand_role(role_key(user.role_id))
  end

  @doc "Returns the role id for the given role key."
  @spec role_id(role_key) :: role_id
  for {id, {key, _desc}} <- @roles do
    def role_id(unquote(key)), do: unquote(id)
  end

  @doc "Returns the role description of the given user."
  @spec role_description(t | Changeset.t) :: String.t
  def role_description(user) do
    {_key, desc} = Map.fetch!(all_roles(), user.role_id)
    desc
  end

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
    |> validate_change(:role_id, &validate_role_id/2)
    |> possibly_update_password_hash
    |> unique_constraint(:email)
  end

  @doc "Validates the user password"
  @spec validate_password(nil | t, String.t) :: boolean
  def validate_password(nil, _password), do: Hash.dummy_checkpw
  def validate_password(user, password) do
    Hash.checkpw(password, user.hashed_password)
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp possibly_update_password_hash(%Changeset{valid?: true, changes: %{password: password}} = changeset)
      when password != "" do
    put_change(changeset, :hashed_password, Hash.hashpwsalt(password))
  end
  defp possibly_update_password_hash(changeset), do: changeset

  defp validate_role_id(:role_id, role_id) do
    if Map.has_key?(all_roles(), role_id) do
      []
    else
      [role_id: "unknown role specified"]
    end
  end

  for {_id, {key, _desc}} <- @roles do
    all_roles = [key | Map.fetch!(@included_roles, key)]
    defp expand_role(unquote(key)), do: unquote(all_roles)
  end

  defp role_key(role_id) do
    {key, _desc} = Map.fetch!(all_roles(), role_id)
    key
  end
end
