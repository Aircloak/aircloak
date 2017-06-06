defmodule Air.Service.User do
  @moduledoc "Service module for working with users"

  alias Air.{Repo, Service.AuditLog, Schemas.Group, Schemas.User}
  import Ecto.Query, only: [from: 2]
  import Ecto.Changeset

  @required_fields ~w(email name)a
  @optional_fields ~w(password password_confirmation)a


  #-----------------------------------------------------------------------------------------------------------
  # API functions
  #-----------------------------------------------------------------------------------------------------------

  @doc "Authenticates the given user."
  @spec login(String.t, String.t, %{atom => any}) ::
    {:ok, User.t} | {:error, :invalid_email_or_password}
  def login(email, password, meta \\ %{}) do
    user = Repo.get_by(User, email: email)
    if User.validate_password(user, password) do
      AuditLog.log(user, "Logged in", meta)
      {:ok, user}
    else
      AuditLog.log(user, "Failed login", meta)
      {:error, :invalid_email_or_password}
    end
  end

  @doc "Returns a list of all users in the system."
  @spec all() :: [User.t]
  def all(), do:
    Repo.all(from user in User, preload: [:groups])

  @doc "Given a list of email addresses, loads the corresponding users."
  @spec by_emails([String.t]) :: [User.t]
  def by_emails(emails), do:
    Repo.all(from user in User, where: user.email in ^emails)

  @doc "Loads the user with the given id."
  @spec load(pos_integer) :: User.t | nil
  def load(user_id), do:
    Repo.one(from user in User, where: user.id == ^user_id, preload: [:groups])

  @doc "Creates the new user, raises on error."
  @spec create!(map) :: User.t
  def create!(params) do
    {:ok, user} = create(params)
    user
  end

  @doc "Creates the new user from the given parameters."
  @spec create(map) :: {:ok, User.t} | {:error, Changeset.t}
  def create(params), do:
    %User{}
    |> user_changeset(params)
    |> Repo.insert()

  @doc "Creates the onboarding admin user."
  @spec create_onboarding_admin_user(map) :: {:ok, User.t} | {:error, Changeset.t}
  def create_onboarding_admin_user(params) do
    changeset =
      %User{}
      |> user_changeset(params["user"])
      |> validate_required([:password, :password_confirmation])

    if params["user"]["master_password"] == Air.site_setting("master_password") do
      group = get_admin_group()
      changeset = user_changeset(changeset, %{groups: [group.id]})
      Repo.insert(changeset)
    else
      changeset = add_error(changeset, :master_password, "The master password is incorrect")
      # We need to trick add the action being performed, to get the form to render errors
      {:error, %{changeset | action: :insert}}
    end
  end

  @doc "Updates the given user, raises on error."
  @spec update!(User.t, map) :: User.t
  def update!(user, params) do
    {:ok, user} = update(user, params)
    user
  end

  @doc "Updates the given user."
  @spec update(User.t, map) :: {:ok, User.t} | {:error, Changeset.t}
  def update(user, params), do:
    user
    |> user_changeset(params)
    |> Repo.update()

  @doc "Deletes the given user."
  @spec delete!(User.t) :: User.t
  def delete!(user), do:
    Repo.delete!(user)

  @doc "Returns the empty changeset for the new user."
  @spec empty_changeset() :: Changeset.t
  def empty_changeset(), do:
    user_changeset(%User{}, %{})

  @doc "Converts the user into a changeset."
  @spec to_changeset(User.t) :: Changeset.t
  def to_changeset(user), do:
    user_changeset(user, %{})

  @doc "Computes the number of data sources accessible by each user."
  @spec data_sources_count() :: %{pos_integer => non_neg_integer}
  def data_sources_count(), do:
    from(
      user in User,
      inner_join: group in assoc(user, :groups),
      inner_join: data_source in assoc(group, :data_sources),
      group_by: user.id,
      select: {user.id, count(data_source.id, :distinct)}
    )
    |> Repo.all()
    |> Enum.into(%{})

  @doc "Returns a boolean regarding whether a administrator account already exists"
  @spec admin_user_exists?() :: boolean
  def admin_user_exists?(), do:
    Repo.one(from u in User, inner_join: g in assoc(u, :groups), where: g.admin, limit: 1) != nil

  #-----------------------------------------------------------------------------------------------------------
  # Internal functions
  #-----------------------------------------------------------------------------------------------------------

  defp user_changeset(user, params), do:
    user
    |> cast(params, @required_fields ++ @optional_fields)
    |> validate_required(@required_fields)
    |> validate_format(:email, ~r/@/)
    |> validate_length(:name, min: 2)
    |> validate_length(:password, min: 4)
    |> validate_confirmation(:password)
    |> update_password_hash()
    |> unique_constraint(:email)
    |> PhoenixMTM.Changeset.cast_collection(:groups, Air.Repo, Group)

  defp update_password_hash(%Ecto.Changeset{valid?: true, changes: %{password: password}} = changeset)
      when password != "" do
    put_change(changeset, :hashed_password, Comeonin.Pbkdf2.hashpwsalt(password))
  end
  defp update_password_hash(changeset), do: changeset

  defp get_admin_group() do
    case Repo.all(from g in Group, where: g.admin) do
      [] -> create_admin_group()
      [group | _] -> group
    end
  end

  defp create_admin_group(), do:
    %Group{}
    |> Group.changeset(%{name: "Admin", admin: true})
    |> Repo.insert!()
end
