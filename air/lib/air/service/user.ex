defmodule Air.Service.User do
  @moduledoc "Service module for working with users"

  alias Air.Repo
  alias Air.Service.{AuditLog, LDAP}
  alias Air.Schemas.{DataSource, Group, User}
  import Ecto.Query, only: [from: 2]
  import Ecto.Changeset

  @required_fields ~w(login name)a
  @password_fields ~w(password password_confirmation)a
  @ldap_fields ~w(ldap_dn source)a
  @ldap_required_fields ~w(ldap_dn)a
  @optional_fields ~w(decimal_sep decimal_digits thousand_sep)a
  @password_reset_salt "4egg+HOtabCGwsCsRVEBIg=="

  @type change_options :: [ldap: true | false | :any]

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Authenticates the given user."
  @spec login(String.t(), String.t(), %{atom => any}) :: {:ok, User.t()} | {:error, :invalid_login_or_password}
  def login(login, password, meta \\ %{}) do
    user = Repo.one(from(u in User, where: u.login == ^login, where: u.enabled))

    cond do
      valid_password?(user, password) ->
        AuditLog.log(user, "Logged in", meta)
        {:ok, user}

      user ->
        AuditLog.log(user, "Failed login", meta)
        {:error, :invalid_login_or_password}

      true ->
        {:error, :invalid_login_or_password}
    end
  end

  @doc """
  Performs a database check to validate if a user is enabled or not.
  Useful when a user record exists in memory, and no database operation
  is planned that the check can implicitly be baked into.
  """
  @spec is_enabled?(User.t()) :: boolean
  def is_enabled?(user),
    do: Repo.one(from(user in User, where: user.id == ^user.id, where: user.enabled, select: true)) || false

  @doc "Returns a token that can be used to reset the given user's password."
  @spec reset_password_token(User.t()) :: String.t()
  def reset_password_token(user), do: Phoenix.Token.sign(AirWeb.Endpoint, @password_reset_salt, user.id)

  @doc "Resets the user's password from the given params. The user is identified by the given reset token."
  @spec reset_password(String.t(), Map.t()) :: {:error, :invalid_token} | {:error, Ecto.Changeset.t()} | {:ok, User.t()}
  def reset_password(token, params) do
    one_day = :timer.hours(24)
    one_week = 7 * one_day

    with {:ok, user_id} <- Phoenix.Token.verify(AirWeb.Endpoint, @password_reset_salt, token, max_age: one_week) do
      Repo.get!(User, user_id)
      |> password_reset_changeset(params)
      |> Repo.update()
    else
      _ -> {:error, :invalid_token}
    end
  end

  @doc "Returns a list of all users in the system."
  @spec all() :: [User.t()]
  def all(), do: Repo.all(from(user in User, preload: [:groups]))

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
  def create(params) do
    %User{}
    |> user_changeset(params)
    |> merge(random_password_changeset(%User{}))
    |> Repo.insert()
  end

  @doc "Creates a new LDAP user from the given parameters."
  @spec create_ldap(map) :: {:ok, User.t()} | {:error, Ecto.Changeset.t()}
  def create_ldap(params) do
    %User{}
    |> user_changeset(params)
    |> merge(random_password_changeset(%User{}))
    |> merge(ldap_changeset(%User{}, params))
    |> Repo.insert()
  end

  @doc "Creates the onboarding admin user."
  @spec create_onboarding_admin_user(map) :: {:ok, User.t()} | {:error, Ecto.Changeset.t()}
  def create_onboarding_admin_user(params) do
    changeset = user_changeset(%User{}, params)

    if params["master_password"] == Air.site_setting("master_password") do
      group = get_admin_group()

      changeset =
        user_changeset(changeset, %{groups: [group.id]})
        |> merge(password_reset_changeset(%User{}, params))

      Repo.insert(changeset)
    else
      changeset = add_error(changeset, :master_password, "The master password is incorrect")
      # We need to trick add the action being performed, to get the form to render errors
      {:error, %{changeset | action: :insert}}
    end
  end

  @doc "Updates the given user, raises on error."
  @spec update!(User.t(), map, change_options) :: User.t()
  def update!(user, params, options \\ []) do
    {:ok, user} = update(user, params, options)
    user
  end

  @doc "Updates the given user. Will raise when trying to update an LDAP-based user."
  @spec update(User.t(), map, change_options) ::
          {:ok, User.t()} | {:error, Ecto.Changeset.t() | :forbidden_no_active_admin}
  def update(user, params, options \\ []) do
    check_ldap!(user, options)

    commit_if_active_last_admin(fn ->
      user
      |> user_changeset(params)
      |> Repo.update()
    end)
  end

  @doc "Updates the profile of the given user, validating user's password."
  @spec update_profile(User.t(), map, change_options) :: {:ok, User.t()} | {:error, Ecto.Changeset.t()}
  def update_profile(user, params, options \\ []) do
    check_ldap!(user, options)

    user
    |> user_changeset(Map.take(params, ~w(name login decimal_sep thousand_sep decimal_digits)))
    |> merge(password_changeset(user, params))
    |> Repo.update()
  end

  @doc "Deletes the given user in the background."
  @spec delete_async(User.t(), (() -> any), (any -> any), change_options) :: :ok
  def delete_async(user, success_callback, failure_callback, options \\ []) do
    check_ldap!(user, options)
    commit_if_active_last_admin_async(fn -> Repo.delete(user) end, success_callback, failure_callback)
  end

  @doc "Deletes the given user, raises on error."
  @spec delete!(User.t()) :: User.t()
  def delete!(user) do
    {:ok, user} = delete(user)
    user
  end

  @doc "Deletes the given user."
  @spec delete(User.t()) :: {:ok, User.t()} | {:error, :forbidden_no_active_admin}
  def delete(user), do: commit_if_active_last_admin(fn -> Repo.delete(user) end)

  @doc "Disables a user account"
  @spec disable(User.t(), change_options) :: {:ok, User.t()} | {:error, :forbidden_no_active_admin}
  def disable(user, options \\ []) do
    check_ldap!(user, options)

    commit_if_active_last_admin(fn ->
      user
      |> cast(%{enabled: false}, [:enabled])
      |> Repo.update()
    end)
  end

  @doc "Enables a user account"
  @spec enable!(User.t(), change_options) :: User.t()
  def enable!(user, options \\ []) do
    check_ldap!(user, options)

    user
    |> cast(%{enabled: true}, [:enabled])
    |> Repo.update!()
  end

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
  @spec active_admin_user_exists?() :: boolean
  def active_admin_user_exists?(),
    do: Repo.one(from(u in User, inner_join: g in assoc(u, :groups), where: g.admin, where: u.enabled, limit: 1)) != nil

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

  @doc "Creates a new LDAP group."
  @spec create_ldap_group(map) :: {:ok, Group.t()} | {:error, Ecto.Changeset.t()}
  def create_ldap_group(params) do
    %Group{}
    |> group_changeset(params)
    |> merge(ldap_changeset(%Group{}, params))
    |> Repo.insert()
  end

  @doc "Updates the given group, raises on error."
  @spec update_group!(Group.t(), map, change_options) :: Group.t()
  def update_group!(group, params, options \\ []) do
    {:ok, group} = update_group(group, params, options)
    group
  end

  @doc "Updates the given group."
  @spec update_group(Group.t(), map, change_options) ::
          {:ok, Group.t()} | {:error, Ecto.Changeset.t() | :forbidden_no_active_admin}
  def update_group(group, params, options \\ []) do
    check_ldap!(group, options)

    commit_if_active_last_admin(fn ->
      group
      |> group_changeset(params)
      |> Repo.update()
    end)
  end

  @doc "Deletes the given group, raises on error."
  @spec delete_group!(Group.t(), change_options) :: Group.t()
  def delete_group!(group, options \\ []) do
    {:ok, group} = delete_group(group, options)
    group
  end

  @doc "Deletes the given group."
  @spec delete_group(Group.t(), change_options) :: {:ok, Group.t()} | {:error, :forbidden_no_active_admin}
  def delete_group(group, options \\ []) do
    check_ldap!(group, options)
    commit_if_active_last_admin(fn -> Repo.delete(group) end)
  end

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

  @doc """
  Generates a pseudonymized ID for a user that can be used when sending query metrics
  and other analyst specific metrics to Aircloak.
  If no user is provided, a random ID will be generated and returned.
  """
  @spec pseudonym(User.t()) :: String.t()
  def pseudonym(nil), do: random_string()

  def pseudonym(user) do
    if is_nil(user.pseudonym) do
      reloaded_user = Air.Service.User.load(user.id)

      if is_nil(reloaded_user.pseudonym) do
        pseudonym = random_string()

        user
        |> cast(%{pseudonym: pseudonym}, [:pseudonym])
        |> Repo.update!()

        pseudonym
      else
        reloaded_user.pseudonym
      end
    else
      user.pseudonym
    end
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp valid_password?(nil, password), do: User.validate_password(nil, password)
  defp valid_password?(user = %{source: :native}, password), do: User.validate_password(user, password)
  defp valid_password?(user = %{source: :ldap}, password), do: match?(:ok, LDAP.simple_bind(user.ldap_dn, password))

  defp random_string, do: Base.encode16(:crypto.strong_rand_bytes(10))

  defp user_changeset(user, params),
    do:
      user
      |> cast(params, @required_fields ++ @optional_fields)
      |> validate_required(@required_fields)
      |> validate_length(:name, min: 2)
      |> validate_length(:decimal_sep, is: 1)
      |> validate_length(:thousand_sep, is: 1)
      |> validate_number(:decimal_digits, greater_than_or_equal_to: 1, less_than_or_equal_to: 9)
      |> unique_constraint(:login)
      |> PhoenixMTM.Changeset.cast_collection(:groups, Air.Repo, Group)

  defp ldap_changeset(user, params) do
    user
    |> cast(Map.put(params, :source, :ldap), @ldap_fields)
    |> validate_required(@ldap_required_fields)
  end

  defp random_password_changeset(user) do
    password = :crypto.strong_rand_bytes(64) |> Base.encode64()
    password_reset_changeset(user, %{password: password, password_confirmation: password})
  end

  defp password_reset_changeset(user, params) do
    user
    |> cast(params, @password_fields)
    |> validate_required(@password_fields)
    |> validate_length(:password, min: 10)
    |> validate_confirmation(:password)
    |> update_password_hash()
  end

  defp password_changeset(user, params) do
    old_password_valid = User.validate_password(user, params["old_password"] || "")

    case {params["password"], old_password_valid} do
      {"", _} -> user_changeset(user, %{})
      {nil, _} -> user_changeset(user, %{})
      {_, true} -> password_reset_changeset(user, params)
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
      |> unique_constraint(:name, name: :groups_name_source_index)
      |> PhoenixMTM.Changeset.cast_collection(:users, Repo, User)
      |> PhoenixMTM.Changeset.cast_collection(:data_sources, Repo, DataSource)

  defp get_admin_group() do
    case admin_groups() do
      [] -> create_group!(%{name: "Admin", admin: true})
      [group | _] -> group
    end
  end

  defp commit_if_active_last_admin(fun), do: GenServer.call(__MODULE__, {:commit_if_active_last_admin, fun})

  defp commit_if_active_last_admin_async(fun, success_callback, failure_callback),
    do: GenServer.cast(__MODULE__, {:commit_if_active_last_admin, fun, success_callback, failure_callback})

  defp do_commit_if_retains_an_admin(fun) do
    Repo.transaction(
      fn ->
        case fun.() do
          {:ok, result} ->
            if active_admin_user_exists?() do
              result
            else
              Repo.rollback(:forbidden_no_active_admin)
            end

          {:error, error} ->
            Repo.rollback(error)
        end
      end,
      timeout: :timer.hours(1)
    )
  end

  defp check_ldap!(object, options) do
    case {object.source, Keyword.get(options, :ldap, false)} do
      {_, :any} -> object
      {:ldap, true} -> object
      {:ldap, false} -> raise "Accidental LDAP change"
      {_, true} -> raise "Accidental non-LDAP change"
      _ -> object
    end
  end

  # -------------------------------------------------------------------
  # GenServer callbacks
  # -------------------------------------------------------------------

  use GenServer

  @impl GenServer
  def init(_), do: {:ok, nil}

  @impl GenServer
  def handle_call({:commit_if_active_last_admin, fun}, _from, state),
    do: {:reply, do_commit_if_retains_an_admin(fun), state}

  @impl GenServer
  def handle_cast({:commit_if_active_last_admin, fun, success_callback, failure_callback}, state) do
    case do_commit_if_retains_an_admin(fun) do
      {:ok, _} -> success_callback.()
      {:error, error} -> failure_callback.(error)
    end

    {:noreply, state}
  end

  # -------------------------------------------------------------------
  # Supervision tree
  # -------------------------------------------------------------------

  @doc false
  def child_spec(_arg), do: Aircloak.ChildSpec.gen_server(__MODULE__, [], name: __MODULE__)
end
