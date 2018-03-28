defmodule Air.Service.User do
  @moduledoc "Service module for working with users"

  alias Air.{Repo, Service.AuditLog, Schemas.DataSource, Schemas.Group, Schemas.User}
  alias Air.Service.{Query, View}
  import Ecto.Query, only: [from: 2]
  import Ecto.Changeset

  @required_fields ~w(email name)a
  @optional_fields ~w(password password_confirmation decimal_sep decimal_digits thousand_sep)a

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Authenticates the given user."
  @spec login(String.t(), String.t(), %{atom => any}) :: {:ok, User.t()} | {:error, :invalid_email_or_password}
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
  @spec all() :: [User.t()]
  def all(), do: Repo.all(from(user in User, preload: [:groups]))

  @doc "Given a list of email addresses, loads the corresponding users."
  @spec by_emails([String.t()]) :: [User.t()]
  def by_emails(emails), do: Repo.all(from(user in User, where: user.email in ^emails))

  @doc "Loads the user with the given id."
  @spec load(pos_integer) :: User.t() | nil
  def load(user_id), do: Repo.one(from(user in User, where: user.id == ^user_id, preload: [:groups]))

  @doc "Creates the new user, raises on error."
  @spec create!(map) :: User.t()
  def create!(params) do
    {:ok, user} = create(params)
    user
  end

  @doc "Creates the new user from the given parameters."
  @spec create(map) :: {:ok, User.t()} | {:error, Ecto.Changeset.t()}
  def create(params),
    do:
      %User{}
      |> user_changeset(params, additional_required_fields: [:password, :password_confirmation])
      |> Repo.insert()

  @doc "Creates the onboarding admin user."
  @spec create_onboarding_admin_user(map) :: {:ok, User.t()} | {:error, Ecto.Changeset.t()}
  def create_onboarding_admin_user(params) do
    changeset =
      user_changeset(
        %User{},
        params["user"],
        additional_required_fields: [:password, :password_confirmation]
      )

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
  @spec update!(User.t(), map) :: User.t()
  def update!(user, params) do
    {:ok, user} = update(user, params)
    user
  end

  @doc "Updates the given user."
  @spec update(User.t(), map) :: {:ok, User.t()} | {:error, Ecto.Changeset.t() | :forbidden_last_admin_deletion}
  def update(user, params),
    do:
      commit_if_last_admin_not_deleted(fn ->
        user
        |> user_changeset(params)
        |> Repo.update()
      end)

  @doc "Updates the profile of the given user, validating user's password."
  @spec update_profile(User.t(), map) :: {:ok, User.t()} | {:error, Ecto.Changeset.t()}
  def update_profile(user, params),
    do:
      user
      |> user_changeset(Map.take(params, ~w(name email decimal_sep thousand_sep decimal_digits)))
      |> merge(password_changeset(user, params))
      |> Repo.update()

  @doc "Deletes the given user, raises on error."
  @spec delete!(User.t()) :: User.t()
  def delete!(user) do
    {:ok, user} = delete(user)
    user
  end

  @doc "Deletes the given user."
  @spec delete(User.t()) :: {:ok, User.t()} | {:error, :forbidden_last_admin_deletion}
  def delete(user),
    do:
      commit_if_last_admin_not_deleted(fn ->
        Query.delete_all(user)
        View.delete_for_user(user)
        Repo.delete(user)
      end)

  @doc "Returns the empty changeset for the new user."
  @spec empty_changeset() :: Ecto.Changeset.t()
  def empty_changeset(), do: user_changeset(%User{}, %{})

  @doc "Converts the user into a changeset."
  @spec to_changeset(User.t()) :: Ecto.Changeset.t()
  def to_changeset(user), do: user_changeset(user, %{})

  @doc "Computes the number of data sources accessible by each user."
  @spec data_sources_count() :: %{pos_integer => non_neg_integer}
  def data_sources_count(),
    do:
      from(
        user in User,
        inner_join: group in assoc(user, :groups),
        inner_join: data_source in assoc(group, :data_sources),
        group_by: user.id,
        select: {user.id, count(data_source.id, :distinct)}
      )
      |> Repo.all()
      |> Enum.into(%{})

  @doc "Returns all users who have access to the given data source."
  @spec data_source_users(DataSource.t()) :: [User.t()]
  def data_source_users(data_source),
    do:
      Repo.all(
        from(
          user in User,
          distinct: user.id,
          inner_join: group in assoc(user, :groups),
          inner_join: data_source in assoc(group, :data_sources),
          where: data_source.id == ^data_source.id,
          select: user,
          preload: [:groups]
        )
      )

  @doc "Returns a boolean regarding whether a administrator account already exists"
  @spec admin_user_exists?() :: boolean
  def admin_user_exists?(),
    do: Repo.one(from(u in User, inner_join: g in assoc(u, :groups), where: g.admin, limit: 1)) != nil

  @doc "Creates the new group, raises on error."
  @spec create_group!(map) :: Group.t()
  def create_group!(params) do
    {:ok, group} = create_group(params)
    group
  end

  @doc "Creates the new group from the given parameters."
  @spec create_group(map) :: {:ok, Group.t()} | {:error, Ecto.Changeset.t()}
  def create_group(params),
    do:
      %Group{}
      |> group_changeset(params)
      |> Repo.insert()

  @doc "Updates the given group, raises on error."
  @spec update_group!(Group.t(), map) :: Group.t()
  def update_group!(group, params) do
    {:ok, group} = update_group(group, params)
    group
  end

  @doc "Updates the given group."
  @spec update_group(Group.t(), map) :: {:ok, Group.t()} | {:error, Ecto.Changeset.t() | :forbidden_last_admin_deletion}
  def update_group(group, params),
    do:
      commit_if_last_admin_not_deleted(fn ->
        group
        |> group_changeset(params)
        |> Repo.update()
      end)

  @doc "Deletes the given group, raises on error."
  @spec delete_group!(Group.t()) :: Group.t()
  def delete_group!(group) do
    {:ok, group} = delete_group(group)
    group
  end

  @doc "Deletes the given group."
  @spec delete_group(Group.t()) :: {:ok, Group.t()} | {:error, :forbidden_last_admin_deletion}
  def delete_group(group), do: commit_if_last_admin_not_deleted(fn -> Repo.delete(group) end)

  @doc "Loads the group with the given id."
  @spec load_group(pos_integer) :: Group.t() | nil
  def load_group(group_id),
    do: Repo.one(from(group in Group, where: group.id == ^group_id, preload: [:users, :data_sources]))

  @doc "Returns a list of all groups in the system."
  @spec all_groups() :: [Group.t()]
  def all_groups(), do: Repo.all(from(group in Group, preload: [:users, :data_sources]))

  @doc "Returns the empty changeset for the new group."
  @spec empty_group_changeset() :: Ecto.Changeset.t()
  def empty_group_changeset(), do: group_changeset(%Group{}, %{})

  @doc "Converts a group into a changeset."
  @spec group_to_changeset(Group.t()) :: Ecto.Changeset.t()
  def group_to_changeset(group), do: group_changeset(group, %{})

  @doc "Returns all admin groups."
  @spec admin_groups() :: [Group.t()]
  def admin_groups(), do: Repo.all(from(g in Group, where: g.admin))

  @doc "Returns the number format settings for the specified user."
  @spec number_format_settings(User.t()) :: Map.t()
  def number_format_settings(user) do
    default_settings = Air.Service.Settings.read()

    %{
      decimal_digits: user.decimal_digits || default_settings.decimal_digits,
      decimal_sep: user.decimal_sep || default_settings.decimal_sep,
      thousand_sep: user.thousand_sep || default_settings.thousand_sep
    }
  end

  @doc "Toggles the debug mode for a user"
  @spec toggle_debug_mode(User.t()) :: User.t()
  def toggle_debug_mode(user) do
    current_debug_mode = user.debug_mode_enabled || false

    user
    |> cast(%{debug_mode_enabled: not current_debug_mode}, [:debug_mode_enabled])
    |> Repo.update!()
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp user_changeset(user, params, opts \\ []),
    do:
      user
      |> cast(params, @required_fields ++ @optional_fields)
      |> validate_required(@required_fields ++ Keyword.get(opts, :additional_required_fields, []))
      |> validate_format(:email, ~r/@/)
      |> validate_length(:name, min: 2)
      |> validate_length(:decimal_sep, is: 1)
      |> validate_length(:thousand_sep, is: 1)
      |> validate_number(:decimal_digits, greater_than_or_equal_to: 1, less_than_or_equal_to: 9)
      |> validate_length(:password, min: 4)
      |> validate_confirmation(:password)
      |> update_password_hash()
      |> unique_constraint(:email)
      |> PhoenixMTM.Changeset.cast_collection(:groups, Air.Repo, Group)

  defp password_changeset(user, params) do
    old_password_valid = User.validate_password(user, params["old_password"] || "")

    case {params["password"], old_password_valid} do
      {"", _} -> user_changeset(user, %{})
      {nil, _} -> user_changeset(user, %{})
      {_, true} -> user_changeset(user, Map.take(params, ["password", "password_confirmation"]))
      {_, false} -> user_changeset(user, %{}) |> add_error(:old_password, "Password invalid")
    end
  end

  defp update_password_hash(%Ecto.Changeset{valid?: true, changes: %{password: password}} = changeset)
       when password != "" do
    put_change(changeset, :hashed_password, Comeonin.Pbkdf2.hashpwsalt(password))
  end

  defp update_password_hash(changeset), do: changeset

  defp group_changeset(group, params),
    do:
      group
      |> cast(params, ~w(name admin)a)
      |> validate_required(~w(name admin)a)
      |> unique_constraint(:name)
      |> PhoenixMTM.Changeset.cast_collection(:users, Repo, User)
      |> PhoenixMTM.Changeset.cast_collection(:data_sources, Repo, DataSource)

  defp get_admin_group() do
    case admin_groups() do
      [] -> create_group!(%{name: "Admin", admin: true})
      [group | _] -> group
    end
  end

  defp commit_if_last_admin_not_deleted(fun),
    # Ensuring that these operations are serialized, so we can consistently verify that the action doesn't remove the
    # last admin.
    # Note that GenServer would be a more idiomatic approach. Here, we're using `:global` for the following reasons:
    #   1. The synchronized code is quite simple.
    #   2. We don't expect these operations to be executed frequently.
    #   3. The code powered by `:global` is very simple and doesn't require extra modules or changes to the supervision
    #      tree.
    do:
      :global.trans({__MODULE__, :isolated_user_change}, fn ->
        # We wrap the `fun` lambda in transaction, to ensure we can safely undo the operation. If the lambda causes the
        # last admin to be deleted, we can simply rollback any database changes.
        Repo.transaction(fn ->
          case fun.() do
            {:ok, result} ->
              # success -> ensure that we still have an admin
              if admin_user_exists?() do
                result
              else
                # no admin anymore -> undo the changes
                Repo.rollback(:forbidden_last_admin_deletion)
              end

            {:error, error} ->
              # some other kind of error -> rollback and return the error
              Repo.rollback(error)
          end
        end)
      end)
end
