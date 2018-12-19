defmodule Air.Service.User do
  @moduledoc "Service module for working with users"

  alias Air.Repo
  alias Air.Service.{AuditLog, LDAP, Salts, Password}
  alias Air.Schemas.{DataSource, Group, User, Login}
  alias AirWeb.Endpoint
  import Ecto.Query, only: [from: 2, join: 4, where: 3, preload: 3]
  import Ecto.Changeset

  @required_fields ~w(name)a
  @login_fields ~w(login)a
  @app_login_fields ~w(description)a
  @password_fields ~w(password password_confirmation)a
  @ldap_fields ~w(ldap_dn source)a
  @ldap_required_fields ~w(ldap_dn)a
  @format_fields ~w(decimal_sep decimal_digits thousand_sep)a
  @optional_fields @format_fields

  @type change_options :: [ldap: true | false | :any]
  @type login :: String.t()
  @type password :: String.t()

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Authenticates the given user, only allowing the main login to be used."
  @spec login(String.t(), String.t(), %{atom => any}) :: {:ok, User.t()} | {:error, :invalid_login_or_password}
  def login(login, password, meta \\ %{}), do: do_login(login, password, meta, [:main])

  @doc "Authenticates the user identified by the login and password, allowing the main or psql login to be used."
  @spec login_psql(String.t(), String.t(), %{atom => any}) :: {:ok, User.t()} | {:error, :invalid_login_or_password}
  def login_psql(login, password, meta \\ %{}), do: do_login(login, password, meta, [:main, :psql])

  @doc "Returns the main login of the user as a string."
  @spec main_login(User.t()) :: String.t()
  def main_login(user) do
    login = user.logins |> Enum.find(&(&1.login_type == :main))
    login.login
  end

  @doc "Returns all logins associated with a user account"
  @spec logins(User.t()) :: [String.t()]
  def logins(user) do
    user = Air.Repo.preload(user, :logins)
    Enum.map(user.logins, & &1.login)
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
  @spec reset_password_token(User.t(), change_options) :: String.t()
  def reset_password_token(user, options \\ []) do
    check_ldap!(user, options)
    Phoenix.Token.sign(Endpoint, Salts.get(:password_reset), user.id)
  end

  @doc "Resets the user's password from the given params. The user is identified by the given reset token."
  @spec reset_password(String.t(), Map.t()) :: {:error, :invalid_token} | {:error, Ecto.Changeset.t()} | {:ok, User.t()}
  def reset_password(token, params) do
    one_day = :timer.hours(24)
    one_week = 7 * one_day

    with {:ok, user_id} <- Phoenix.Token.verify(Endpoint, Salts.get(:password_reset), token, max_age: one_week) do
      load(user_id)
      |> change_main_login(&password_reset_changeset(&1, params))
      |> update()
    else
      _ -> {:error, :invalid_token}
    end
  end

  @doc "Returns a list of all users in the system."
  @spec all() :: [User.t()]
  def all(), do: Repo.all(from(user in User, preload: [:logins, :groups]))

  @doc "Returns a list of all native users in the system."
  @spec all_native() :: [User.t()]
  def all_native(), do: Repo.all(from(user in User, where: [source: ^:native], preload: [:logins, :groups]))

  @doc "Loads the user with the given id."
  @spec load(pos_integer | binary) :: User.t() | nil
  def load(user_id), do: Repo.one(from(user in User, where: user.id == ^user_id, preload: [:logins, :groups]))

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
    |> merge(random_password_changeset(%User{}, params))
    |> insert()
  end

  @doc "Creates a new LDAP user from the given parameters."
  @spec create_ldap(map) :: {:ok, User.t()} | {:error, Ecto.Changeset.t()}
  def create_ldap(params) do
    %User{}
    |> user_changeset(params)
    |> merge(random_password_changeset(%User{}, params))
    |> merge(ldap_changeset(%User{}, params))
    |> insert()
  end

  @doc "Creates the onboarding admin user."
  @spec create_onboarding_admin_user(map) :: {:ok, User.t()} | {:error, Ecto.Changeset.t()}
  def create_onboarding_admin_user(params) do
    changeset = user_changeset(%User{}, params)

    if params["master_password"] == Air.site_setting!("master_password") do
      group = get_admin_group()

      changeset
      |> user_changeset(%{groups: [group.id]})
      |> merge(change_main_login(%User{}, &full_login_changeset(&1, params)))
      |> insert()
    else
      changeset = add_error(changeset, :master_password, "The master password is incorrect")
      # We need to trick add the action being performed, to get the form to render errors
      {:error, %{changeset | action: :insert}}
    end
  end

  @doc "Creates a new app login."
  @spec create_app_login(User.t(), map) :: {:ok, login, password} | {:error, Ecto.Changeset.t()}
  def create_app_login(user, params) do
    login = "#{main_login(user)}-#{random_string()}"
    password = random_password()

    user
    |> Ecto.build_assoc(:logins)
    |> change(%{login_type: :psql, login: login, hashed_password: Password.hash(password)})
    |> unique_constraint(:login)
    |> cast(params, @app_login_fields)
    |> Repo.insert()
    |> case do
      {:ok, _} -> {:ok, login, password}
      {:error, changeset} -> {:error, changeset}
    end
  end

  @doc """
  Delete the app login of the given user with the given id. Returns error and doesn't delete if the identified login
  does not belong to that user or is a main login.
  """
  @spec delete_app_login(User.t(), integer() | String.t()) :: {:ok, Login.t()} | :error
  def delete_app_login(user, id) do
    Login
    |> where([l], l.id == ^id)
    |> where([l], l.user_id == ^user.id)
    |> where([l], l.login_type != ^:main)
    |> Repo.one()
    |> case do
      nil -> :error
      login -> Repo.delete(login)
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
      |> merge(change_main_login(user, &main_login_changeset(&1, params)))
      |> update()
    end)
  end

  @doc "Updates the profile of the given user, validating user's password."
  @spec update_full_profile(User.t(), map, change_options) :: {:ok, User.t()} | {:error, Ecto.Changeset.t()}
  def update_full_profile(user, params, options \\ []) do
    check_ldap!(user, options)

    user
    |> user_changeset(Map.take(params, ~w(name decimal_sep thousand_sep decimal_digits)))
    |> merge(
      change_main_login(user, fn login ->
        login
        |> password_changeset(params)
        |> merge(main_login_changeset(login, params))
      end)
    )
    |> update()
  end

  @doc "Updates the profile of the given user, only allowing changes to non-login-related settings, like number format."
  @spec update_profile_settings(User.t(), map) :: {:ok, User.t()} | {:error, Ecto.Changeset.t()}
  def update_profile_settings(user, params) do
    user
    |> number_format_changeset(params)
    |> update()
  end

  @doc """
  Deletes the given user in the background. Calls `start_callback` and returns `:ok` immediately if the user can be
  disabled and the deletion process was started. Calls `success_callback` or `failure_callback` in the background when
  finished.
  """
  @spec delete_async(User.t(), (() -> any), (() -> any), (any -> any)) ::
          :ok | {:error, :forbidden_no_active_admin | :invalid_ldap_delete}
  def delete_async(%User{source: :ldap, enabled: true}, _, _, _), do: {:error, :invalid_ldap_delete}

  def delete_async(user = %User{source: :ldap, enabled: false}, start_callback, success_callback, failure_callback) do
    start_callback.()
    commit_if_active_last_admin_async(fn -> Repo.delete(user) end, success_callback, failure_callback)
  end

  def delete_async(user, start_callback, success_callback, failure_callback) do
    case disable(user) do
      {:ok, user} ->
        start_callback.()
        commit_if_active_last_admin_async(fn -> Repo.delete(user) end, success_callback, failure_callback)

      error ->
        error
    end
  end

  @doc "Deletes the given user, raises on error."
  @spec delete!(User.t()) :: User.t()
  def delete!(user) do
    {:ok, user} = delete(user)
    user
  end

  @doc "Deletes the given user."
  @spec delete(User.t()) :: {:ok, User.t()} | {:error, :forbidden_no_active_admin | :invalid_ldap_delete}
  def delete(%User{source: :ldap, enabled: true}), do: {:error, :invalid_ldap_delete}
  def delete(user), do: commit_if_active_last_admin(fn -> Repo.delete(user) end)

  @doc "Disables a user account"
  @spec disable(User.t(), change_options) :: {:ok, User.t()} | {:error, :forbidden_no_active_admin}
  def disable(user, options \\ []) do
    check_ldap!(user, options)

    commit_if_active_last_admin(fn ->
      user
      |> cast(%{enabled: false}, [:enabled])
      |> update()
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
  def empty_changeset(), do: change(%User{})

  @doc "Returns an empty changeset for a new app login."
  @spec app_login_changeset() :: Ecto.Changeset.t()
  def app_login_changeset(), do: change(%Login{})

  @doc "Converts the user into a changeset."
  @spec to_changeset(User.t()) :: Ecto.Changeset.t()
  def to_changeset(user), do: change(user, %{login: main_login(user)})

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
          preload: [:logins, :groups]
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
      |> insert()

  @doc "Creates a new LDAP group."
  @spec create_ldap_group(map) :: {:ok, Group.t()} | {:error, Ecto.Changeset.t()}
  def create_ldap_group(params) do
    %Group{}
    |> group_changeset(params, ldap: true)
    |> merge(ldap_changeset(%Group{}, params))
    |> insert()
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
      |> group_changeset(params, options)
      |> update()
    end)
  end

  @doc "Updates only the data sources of the given group."
  @spec update_group_data_sources(Group.t(), map) :: {:ok, Group.t()} | {:error, Ecto.Changeset.t()}
  def update_group_data_sources(group, params) do
    group
    |> group_data_source_changeset(params)
    |> update()
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

  @doc "Returns a group by name"
  @spec get_group_by_name(String.t()) :: {:ok, Group.t()} | {:error, :not_found}
  def get_group_by_name(name) do
    case(Air.Repo.get_by(Air.Schemas.Group, name: name)) do
      nil -> {:error, :not_found}
      group -> {:ok, group}
    end
  end

  @doc "Returns the number format settings for the specified user."
  @spec number_format_settings(User.t() | nil) :: Map.t()
  def number_format_settings(nil),
    do: Air.Service.Settings.read() |> Map.take([:decimal_digits, :decimal_sep, :thousand_sep])

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

  @doc "Returns a user by login"
  @spec get_by_login(String.t()) :: {:ok, User.t()} | {:error, :not_found}
  def get_by_login(login) do
    User
    |> join(:left, [user], login in assoc(user, :logins))
    |> where([_user, login], login.login == ^login)
    |> where([_user, login], login.login_type == ^:main)
    |> Repo.one()
    |> Repo.preload([:logins, :groups])
    |> case do
      nil -> {:error, :not_found}
      user -> {:ok, user}
    end
  end

  @doc "Adds a user preconfigured in a users or data source config file."
  @spec add_preconfigured_user(Map.t()) :: {:ok, User.t()} | :error
  def add_preconfigured_user(user_data) do
    changeset =
      %User{}
      |> user_changeset(%{name: user_data.login})
      |> merge(
        change_main_login(%User{}, fn login ->
          login
          |> main_login_changeset(%{login: user_data.login})
          |> put_change(:hashed_password, user_data.password_hash)
        end)
      )

    if Map.get(user_data, :admin, false) do
      user_changeset(changeset, %{groups: [get_admin_group().id]})
    else
      changeset
    end
    |> insert()
    |> case do
      {:error, _} -> :error
      {:ok, _user} = result -> result
    end
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp do_login(login, password, meta, login_types) do
    login =
      mask_timing(fn ->
        Login
        |> join(:left, [login], user in assoc(login, :user))
        |> where([login, _user], login.login == ^login)
        |> where([login, _user], login.login_type in ^login_types)
        |> where([_login, user], user.enabled)
        |> preload([_login, user], user: user)
        |> Repo.one()
      end)

    cond do
      valid_password?(login, password) ->
        mask_timing(fn ->
          AuditLog.log(login.user, "Logged in", meta)
          Air.TimestampUpdater.start_toucher(login)
        end)

        {:ok, login.user}

      login ->
        mask_timing(fn -> AuditLog.log(login.user, "Failed login", meta) end)
        {:error, :invalid_login_or_password}

      true ->
        mask_timing(fn -> :noop end)
        {:error, :invalid_login_or_password}
    end
  end

  defp valid_password?(login, password) do
    case login do
      %{login_type: :main, user: %{source: :ldap}} ->
        {_, result} = {validate_password(login, password), LDAP.simple_bind(login.user.ldap_dn, password)}
        match?(:ok, result)

      _ ->
        {result, _} = {validate_password(login, password), LDAP.dummy_bind()}
        result
    end
  end

  @timing_mask 20
  defp mask_timing(action) do
    :timer.send_after(@timing_mask, :wake_up)
    res = action.()

    receive do
      :wake_up -> res
    after
      2 * @timing_mask -> res
    end
  end

  defp random_string(), do: :crypto.strong_rand_bytes(10) |> Base.encode16()

  defp random_password(), do: :crypto.strong_rand_bytes(64) |> Base.encode64()

  defp user_changeset(user, params) do
    user
    |> cast(params, @required_fields ++ @optional_fields)
    |> validate_required(@required_fields)
    |> validate_length(:name, min: 2)
    |> merge(number_format_changeset(user, params))
    |> PhoenixMTM.Changeset.cast_collection(:groups, Air.Repo, Group)
  end

  defp full_login_changeset(login, params) do
    login
    |> password_reset_changeset(params)
    |> merge(main_login_changeset(login, params))
  end

  defp main_login_changeset(login, params) do
    login
    |> cast(params, @login_fields)
    |> validate_required(@login_fields)
    |> unique_constraint(:login)
  end

  defp number_format_changeset(user, params) do
    user
    |> cast(params, @format_fields)
    |> validate_length(:decimal_sep, is: 1)
    |> validate_length(:thousand_sep, is: 1)
    |> validate_number(:decimal_digits, greater_than_or_equal_to: 1, less_than_or_equal_to: 9)
  end

  defp ldap_changeset(user, params) do
    user
    |> cast(Map.put(params, :source, :ldap), @ldap_fields)
    |> validate_required(@ldap_required_fields)
  end

  defp random_password_changeset(user, params) do
    change_main_login(user, fn login ->
      password = random_password()

      login
      |> password_reset_changeset(%{password: password, password_confirmation: password})
      |> merge(main_login_changeset(login, params))
    end)
  end

  defp password_reset_changeset(login, params) do
    login
    |> cast(params, @password_fields)
    |> validate_required(@password_fields)
    |> validate_length(:password, min: 10)
    |> validate_confirmation(:password)
    |> update_password_hash()
  end

  defp password_changeset(login, params) do
    old_password_valid = validate_password(login, params["old_password"] || "")

    case {params["password"], old_password_valid} do
      {"", _} -> change(login)
      {nil, _} -> change(login)
      {_, true} -> password_reset_changeset(login, params)
      {_, false} -> change(login) |> add_error(:old_password, "Password invalid")
    end
  end

  defp update_password_hash(%Ecto.Changeset{valid?: true, changes: %{password: password}} = changeset)
       when password != "" do
    put_change(changeset, :hashed_password, Password.hash(password))
  end

  defp update_password_hash(changeset), do: changeset

  defp validate_password(nil, password), do: Password.validate(password, nil)
  defp validate_password(user, password), do: Password.validate(password, user.hashed_password)

  defp group_changeset(group, params, options \\ []),
    do:
      group
      |> cast(params, ~w(name admin)a)
      |> validate_required(~w(name admin)a)
      |> unique_constraint(:name, name: :groups_name_source_index)
      |> PhoenixMTM.Changeset.cast_collection(:users, Repo, User)
      |> validate_change(:users, &validate_group_user_source(&1, &2, options))
      |> PhoenixMTM.Changeset.cast_collection(:data_sources, Repo, DataSource)

  defp group_data_source_changeset(group, params) do
    group
    |> cast(params, [])
    |> PhoenixMTM.Changeset.cast_collection(:data_sources, Repo, DataSource)
  end

  defp validate_group_user_source(:users, users, options) do
    valid_source = if(Keyword.get(options, :ldap, false), do: :ldap, else: :native)
    invalid_users = Enum.filter(users, &(&1.data.source != valid_source))

    case {valid_source, invalid_users} do
      {_, []} -> []
      {:native, _} -> [users: "cannot assign LDAP users to a native group"]
      {:ldap, _} -> [users: "cannot assign native users to an LDAP group"]
    end
  end

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

  defp change_main_login(changeset = %Ecto.Changeset{changes: %{logins: [login_changeset]}}, action) do
    login_changeset
    |> action.()
    |> merge_login_changeset(changeset)
  end

  defp change_main_login(new_user = %User{id: nil}, action) do
    %Login{}
    |> action.()
    |> change(%{login_type: :main})
    |> merge_login_changeset(change(new_user))
  end

  defp change_main_login(existing_user, action) do
    main_login = Enum.find(existing_user.logins, &(&1.login_type == :main))
    main_login_changeset = action.(main_login)

    changeset_mapper = fn
      _, %{login_type: :main} -> main_login_changeset
      _, login -> change(login)
    end

    if main_login_changeset.valid? do
      existing_user
      |> cast(%{logins: existing_user.logins}, [])
      |> cast_assoc(:logins, with: changeset_mapper)
    else
      merge_login_changeset(main_login_changeset, change(existing_user))
    end
  end

  defp insert(changeset), do: changeset |> Repo.insert() |> merge_login_errors()

  defp update(changeset), do: changeset |> Repo.update() |> merge_login_errors()

  defp merge_login_errors({:error, changeset = %{changes: %{logins: [%{errors: login_errors}]}}}),
    do: {:error, update_in(changeset, [Access.key(:errors)], fn errors -> errors ++ login_errors end)}

  defp merge_login_errors(other), do: other

  defp merge_login_changeset(login_changeset, user_changeset) do
    if login_changeset.valid? do
      put_assoc(user_changeset, :logins, [login_changeset])
    else
      Enum.reduce(login_changeset.errors, user_changeset, fn {field, {msg, opts}}, changeset ->
        add_error(changeset, field, msg, opts)
      end)
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
