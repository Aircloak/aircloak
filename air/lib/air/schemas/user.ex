defmodule Air.Schemas.User do
  @moduledoc "The user model."
  require Logger

  use Air.Schemas.Base

  alias Ecto.Changeset
  alias Comeonin.Pbkdf2, as: Hash
  alias Air.Schemas.Group

  @type t :: %__MODULE__{}
  @type role_key :: :anonymous | :user | :admin
  @type operation :: atom
  @type permissions :: %{role_key => [operation] | :all}

  schema "users" do
    field :email, :string
    field :hashed_password, :string
    field :name, :string

    has_many :queries, Air.Schemas.Query
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

  @required_fields ~w(email name)a
  @optional_fields ~w(password password_confirmation)a

  @roles %{
    0 => {:user, "user"},
    1 => {:admin, "administrator"}
  }

  @included_roles %{
    user: [:anonymous],
    admin: [:user, :anonymous]
  }


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Returns all user's roles."
  @spec roles(nil | t) :: [role_key]
  def roles(nil),
    do: [:anonymous]
  def roles(user) do
    if admin?(user) do
      expand_role(:admin)
    else
      expand_role(:user)
    end
  end

  @doc """
  Returns true if the user belongs to the administrator role.
  Note that the groups association needs to be preloaded before calling this method.
  """
  @spec admin?(nil | t) :: boolean
  def admin?(nil), do: false
  def admin?(user), do: Enum.any?(user.groups, &(&1.admin))

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
    |> validate_length(:password, min: 4)
    |> validate_confirmation(:password)
    |> update_password_hash
    |> unique_constraint(:email)
    |> PhoenixMTM.Changeset.cast_collection(:groups, Air.Repo, Group)
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

  @doc "Returns a boolean regarding whether a administrator account already exists"
  @spec admin_user_exists?() :: boolean
  def admin_user_exists?() do
    query = from u in Air.Schemas.User,
      inner_join: g in assoc(u, :groups),
      where: g.admin,
      limit: 1
    Air.Repo.one(query) != nil
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp update_password_hash(%Changeset{valid?: true, changes: %{password: password}} = changeset)
      when password != "" do
    put_change(changeset, :hashed_password, Hash.hashpwsalt(password))
  end
  defp update_password_hash(changeset), do: changeset

  for {_id, {key, _desc}} <- @roles do
    all_roles = [key | Map.get(@included_roles, key, [])]
    defp expand_role(unquote(key)), do: unquote(all_roles)
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
