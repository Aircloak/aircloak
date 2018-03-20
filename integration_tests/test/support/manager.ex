defmodule IntegrationTest.Manager do
  import Ecto.Query, only: [from: 2]

  alias Air.Repo
  alias Air.Schemas.{DataSource, ExportForAircloak, Group, Query, ResultChunk, User, View}

  @admin_group_name "admins"
  @user_password "1234"
  @data_source_name "data_source_name"

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

  def user_password(), do: @user_password

  def create_air_user() do
    admin_group = Repo.one!(from group in Group, where: group.name == @admin_group_name)

    # create user
    Air.Service.User.create!(%{
      email: "user_#{:erlang.unique_integer([:positive])}@aircloak.com",
      name: "user_#{:erlang.unique_integer([:positive])}",
      password: @user_password,
      password_confirmation: @user_password,
      groups: [admin_group.id]
    })
  end

  def load_valid_license(), do:
    :ok = create_license() |> Central.Service.License.export() |> Air.Service.License.load()

  def load_expired_license() do
    license = create_license()
    {:ok, _} = Ecto.Adapters.SQL.query(
      Central.Repo,
      "UPDATE licenses SET inserted_at = '2000-01-01 00:00:00' WHERE id = $1",
      [license.id]
    )

    :ok =
      Central.Schemas.License
      |> Central.Repo.get!(license.id)
      |> Central.Service.License.export()
      |> Air.Service.License.load()
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp await_data_source() do
    if is_nil(data_source()) do
      :timer.sleep(100)
      await_data_source()
    end
  end

  defp setup_cloak_database() do
    Cloak.Test.DB.start_link()
    :ok = Cloak.Test.DB.create_table("users", "name TEXT, height INTEGER")
    :ok = insert_rows(1..100, "users", ["name", "height"], ["john", 180])
  end

  defp setup_air_database() do
    # delete previous entries
    Repo.delete_all(View)
    Repo.delete_all("data_sources_groups")
    Repo.delete_all("groups_users")
    Repo.delete_all(ResultChunk)
    Repo.delete_all(Query)
    Repo.delete_all(User)
    Repo.delete_all(Group)

    # create group
    admin_group = Air.Service.User.create_group!(%{name: @admin_group_name, admin: true})

    # connect data source to group
    from(data_source in DataSource, where: data_source.name == @data_source_name)
    |> Repo.one!()
    |> Repo.preload([:groups])
    |> Air.Service.DataSource.update!(%{groups: [admin_group.id], name: @data_source_name})

    load_valid_license()

    Repo.delete_all(ExportForAircloak)
  end

  defp insert_rows(user_id_range, table, columns, values) do
    Cloak.Test.DB.add_users_data(table, columns, Enum.map(user_id_range, &["user#{&1}" | values]))
  end

  defp setup_central() do
    Central.Repo.delete_all(Central.Schemas.License)
    Central.Repo.delete_all(Central.Schemas.AirRPC)
    Central.Repo.delete_all(Central.Schemas.CustomerExport)
    Central.Repo.delete_all(Central.Schemas.Customer)
    {:ok, _} = Central.Service.Customer.create(%{name: "integration tests customer"})
  end

  defp create_license() do
    {:ok, license} =
      Central.Schemas.Customer
      |> Central.Repo.one!()
      |> Central.Service.License.create(%{name: "test license", length_in_days: 10, auto_renew: false})
    license
  end
end
