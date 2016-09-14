defmodule Air.User do
  @moduledoc "The user model."
  require Logger

  use Air.Web, :model

  alias Ecto.Changeset
  alias Comeonin.Pbkdf2, as: Hash
  alias Air.{Organisation, Query, Group}

  @type t :: %__MODULE__{}
  @type role_id :: non_neg_integer
  @type role_key :: :anonymous | :user | :org_admin | :admin
  @type operation :: atom
  @type permissions :: %{role_key => [operation] | :all}

  schema "users" do
    field :email, :string
    field :hashed_password, :string
    field :name, :string
    field :role_id, :integer

    belongs_to :organisation, Organisation

    has_many :queries, Query
    many_to_many :groups, Group,
      join_through: "groups_users",
      on_delete: :delete_all,
      on_replace: :delete

    timestamps

    # These virtual fields are used for validation,
    # but never persisted to the database
    field :password, :string, virtual: true
    field :password_confirmation, :string, virtual: true
  end

  @required_fields ~w(email name organisation_id role_id)a
  @optional_fields ~w(password password_confirmation)a

  @roles %{
    0 => {:user, "user"},
    1 => {:org_admin, "organisation manager"},
    2 => {:admin, "administrator"}
  }

  @included_roles %{
    user: [:anonymous],
    org_admin: [:user, :anonymous],
    admin: [:org_admin, :user, :anonymous]
  }


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Returns a list of all supported roles."
  @spec all_roles :: %{role_id => {role_key, description::String.t}}
  def all_roles, do: @roles

  @doc """
  Returns all user's roles.

  Note: You need to preload the organisation association before calling this
  function.
  """
  @spec roles(nil | t) :: [role_key]
  def roles(nil),
    do: [:anonymous]
  def roles(%{organisation: %Ecto.Association.NotLoaded{}}),
    do: raise "organisation is not preloaded"
  def roles(%{organisation: org, role_id: role_id}) do
    if Organisation.admins?(org) do
      # user in the administrators group is always the admin
      expand_role(:admin)
    else
      expand_role(role_key(role_id))
    end
  end

  @doc "Returns true if the user belongs to the administrator role."
  @spec admin?(nil | t) :: boolean
  def admin?(user),
    do: Enum.member?(roles(user), :admin)

  @doc "Returns the role id for the given role key."
  @spec role_id(role_key) :: role_id
  for {id, {key, _desc}} <- @roles do
    def role_id(unquote(key)), do: unquote(id)
  end

  @doc "Returns the role description of the given user."
  @spec role_description(t) :: String.t
  def role_description(user) do
    {_key, desc} = Map.fetch!(all_roles(), user.role_id)
    desc
  end

  @doc "Verifies whether the provided user has permission for the given operation"
  @spec permitted?(nil | t, operation, permissions) :: boolean
  def permitted?(user, operation, permissions) do
    user
    |> roles()
    |> Stream.map(&Map.get(permissions, &1, []))
    |> Enum.any?(
      fn
        :all -> true
        allowed -> Enum.member?(allowed, operation)
      end
    )
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
    |> validate_format(:email, ~r/@/)
    |> validate_length(:name, min: 2)
    |> validate_confirmation(:password)
    |> validate_change(:role_id, &validate_role_id/2)
    |> update_password_hash
    |> unique_constraint(:email)
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

  defp validate_role_id(:role_id, role_id) do
    if role_id != role_id(:admin) and Map.has_key?(all_roles(), role_id) do
      []
    else
      [role_id: "invalid role specified"]
    end
  end

  for {_id, {key, _desc}} <- @roles do
    all_roles = [key | Map.get(@included_roles, key, [])]
    defp expand_role(unquote(key)), do: unquote(all_roles)
  end

  defp role_key(role_id) do
    {key, _desc} = Map.fetch!(all_roles(), role_id)
    key
  end

  defimpl Inspect do
    @moduledoc """
    Custom inspection of the user record, where we only present non-sensitive fields.

    This allows us to safely inspect user anywhere in log expressions without
    worrying we'll leek some sensitive data.
    """
    def inspect(user, opts) do
      Inspect.Map.inspect(Map.take(user, [:id]), Inspect.Atom.inspect(user.__struct__), opts)
    end
  end
end
