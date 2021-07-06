defmodule IntegrationTest.Manager do
  import Ecto.Query, only: [from: 2]
  import IntegrationTest.Helpers
  import ExUnit.Assertions
  import Aircloak.AssertionHelper

  alias Air.Repo
  alias Air.Schemas.{AnalystTable, DataSource, Group, Query, ResultChunk, User, View}

  @admin_group_name "admins"
  @user_password "psswrd12"
  @data_source_name "postgresql"

  def start(_type, _args) do
    {:ok, _} = Application.ensure_all_started(:central)
    setup_central()

    # Setting up the cloak database injects the tables into
    # the cloak data source config. This needs to happen
    # prior to the cloak sendings its data sources to the air.
    # Otherwise the air operates with stale data source definitions
    {:ok, _} = Application.ensure_all_started(:cloak)
    setup_cloak_database()

    {:ok, _} = Application.ensure_all_started(:air)
    await_data_source()
    setup_air_database()

    # dummy supervisor to have a top-level process
    Supervisor.start_link([], strategy: :one_for_one)
  end

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  def data_source_name(), do: @data_source_name

  def data_source(), do: Repo.get_by(DataSource, name: @data_source_name)

  def default_admin() do
    {:ok, admin} = Air.Service.User.get_by_login("admin@aircloak.com")
    admin
  end

  def user_password(), do: @user_password

  def login(user), do: Air.Service.User.main_login(user)

  def admin_group(), do: Repo.one!(from(group in Group, where: group.name == @admin_group_name))

  def create_admin_user(), do: create_air_user(admin_group())

  def create_air_user(group) do
    user_name = unique_name(:user)
    user = Air.Service.User.create!(%{login: "#{user_name}@aircloak.com", name: user_name, groups: [group.id]})
    token = Air.Service.User.reset_password_token(user)

    {:ok, user} =
      Air.Service.User.reset_password(token, %{
        password: @user_password,
        password_confirmation: @user_password
      })

    ExUnit.Callbacks.on_exit(fn ->
      with {:ok, user} <- Air.Service.User.load(user.id), do: Air.Service.User.delete(user)
    end)

    user
  end

  def restart_cloak() do
    Application.stop(:cloak)
    ensure_cloak_disconnected()

    Application.start(:cloak)
    :ok = create_users_table(skip_db_create: true)
    :ok = create_column_access_table(skip_db_create: true)
    :ok = create_integers(skip_db_create: true)
    ensure_cloak_connected(expected_table_count: 3)
  end

  def reset_air() do
    ExUnit.CaptureIO.capture_io(:stderr, fn ->
      Ecto.Migrator.run(
        Air.Repo,
        Application.app_dir(:air, "priv/repo/migrations"),
        :down,
        all: true
      )

      Application.stop(:air)

      :jobs.delete_queue(Air.PsqlServer.ShadowDb.Manager)
      Application.start(:air)
      await_data_source()
    end)
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp await_data_source(count \\ 0)

  defp await_data_source(1200), do: raise("Timeout while waiting for data source.")

  defp await_data_source(count) do
    if is_nil(data_source()) do
      :timer.sleep(100)
      await_data_source(count + 1)
    end
  end

  defp setup_cloak_database() do
    Cloak.Test.DB.start_link()
    :ok = create_users_table()
    :ok = insert_rows(1..100, "users", ["name", "height"], ["john", 180])

    :ok = create_column_access_table()

    :ok =
      Cloak.Test.DB.insert_data(
        "column_access",
        ~w/user_id white grey black/,
        Enum.map(1..1_000, &["user_1", &1, &1, &1])
      )

    :ok = create_integers()

    :ok =
      Cloak.Test.DB.insert_data(
        "integers",
        ~w/user_id value/,
        Enum.map(1..1_000, &["user_1", &1])
      )
  end

  defp create_users_table(opts \\ []) do
    Cloak.Test.DB.create_table("users", "name TEXT, height INTEGER", opts)
  end

  defp create_integers(opts \\ []) do
    Cloak.Test.DB.create_table("integers", "value INTEGER", opts)
  end

  defp create_column_access_table(opts \\ []) do
    Cloak.Test.DB.create_table(
      "column_access",
      "white INTEGER, grey INTEGER, black INTEGER",
      Keyword.merge(
        opts,
        unselectable_columns: ["grey"],
        exclude_columns: ["black"]
      )
    )
  end

  def setup_air_database() do
    # delete previous entries
    Repo.delete_all(View)
    Repo.delete_all("data_sources_groups")
    Repo.delete_all("groups_users")
    Repo.delete_all(ResultChunk)
    Repo.delete_all(Query)
    Repo.delete_all(AnalystTable)
    Repo.delete_all(User)
    Repo.delete_all(Group)

    # create group
    admin_group = Air.Service.Group.create!(%{name: @admin_group_name, admin: true})

    {:ok, _aircloak_admin} =
      Air.Service.User.create_onboarding_admin_user(%{
        "master_password" => "super_secret_master_password",
        "name" => "aircloak_admin",
        "login" => "admin@aircloak.com",
        "password" => @user_password,
        "password_confirmation" => @user_password
      })

    # connect data source to group
    from(data_source in DataSource, where: data_source.name == @data_source_name)
    |> Repo.one!()
    |> Repo.preload([:groups])
    |> Air.Service.DataSource.update!(%{groups: [admin_group.id], name: @data_source_name})

    Air.Service.PrivacyPolicy.set("Privacy policy content")
  end

  defp insert_rows(user_id_range, table, columns, values) do
    Cloak.Test.DB.insert_data(
      table,
      ["user_id" | columns],
      Enum.map(user_id_range, &["user#{&1}" | values])
    )
  end

  defp setup_central() do
    Central.Repo.delete_all(Central.Schemas.License)
    Central.Repo.delete_all(Central.Schemas.Customer)
    {:ok, _} = Central.Service.Customer.create(%{name: "integration tests customer"})
  end

  defp create_license() do
    {:ok, license} =
      Central.Schemas.Customer
      |> Central.Repo.one!()
      |> Central.Service.License.create(%{
        name: "test license",
        length_in_days: 10,
        auto_renew: false
      })

    license
  end

  defp ensure_cloak_disconnected() do
    assert_soon(
      [] = Air.Service.Cloak.channel_pids(@data_source_name),
      timeout: :timer.seconds(5)
    )
  end

  defp ensure_cloak_connected(expected_table_count: expected_table_count) do
    assert_soon(
      expected_table_count ==
        Air.Service.DataSource.by_name(@data_source_name)
        |> DataSource.tables()
        |> Enum.count(),
      timeout: :timer.seconds(5)
    )
  end
end
