defmodule Air.Schemas.User do
  @moduledoc "The user model."
  require Logger

  use Air.Schemas.Base

  alias Comeonin.Pbkdf2, as: Hash
  alias Air.Schemas.{Group, PrivacyPolicy}

  @type t :: %__MODULE__{}
  @type role_key :: :anonymous | :user | :admin
  @type operation :: atom
  @type permissions :: %{role_key => [operation] | :all}

  schema "users" do
    field(:email, :string)
    field(:hashed_password, :string)
    field(:name, :string)

    has_many(:queries, Air.Schemas.Query)

    many_to_many(
      :groups,
      Group,
      join_through: "groups_users",
      on_delete: :delete_all,
      on_replace: :delete
    )

    has_many(:views, Air.Schemas.View)

    belongs_to(:accepted_privacy_policy, PrivacyPolicy)

    timestamps()

    # number format overrides
    field(:decimal_sep, :string)
    field(:thousand_sep, :string)
    field(:decimal_digits, :integer)

    field(:debug_mode_enabled, :boolean)

    # These virtual fields are used for validation,
    # but never persisted to the database
    field(:password, :string, virtual: true)
    field(:password_confirmation, :string, virtual: true)
  end

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
  def roles(nil), do: [:anonymous]

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
  def admin?(user), do: Enum.any?(user.groups, & &1.admin)

  @doc "Verifies whether the provided user has permission for the given operation"
  @spec permitted?(nil | t, operation, permissions) :: boolean
  def permitted?(user, operation, permissions) do
    user
    |> roles()
    |> Stream.map(&Map.get(permissions, &1, []))
    |> Enum.any?(fn
      :all -> true
      allowed -> Enum.member?(allowed, operation)
    end)
  end

  @doc "Validates the user password."
  @spec validate_password(nil | t, String.t()) :: boolean
  def validate_password(nil, _password), do: Hash.dummy_checkpw()
  def validate_password(user, password), do: Hash.checkpw(password, user.hashed_password)

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

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
      Inspect.Map.inspect(Map.take(user, [:id]), inspect(user.__struct__), opts)
    end
  end
end
