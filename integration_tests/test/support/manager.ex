defmodule IntegrationTest.Manager do
  import Ecto.Query, only: [from: 2]

  alias Air.Repo
  alias Air.Schemas.{DataSource, Group, User, View}

  @admin_group_name "admins"
  @user_password "1234"
  @data_source_global_id "postgres/cloaktest1-native@localhost"


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  def setup() do
    await_data_source()
    setup_cloak_database()
    setup_data_source()
  end

  def data_source_global_id(), do: @data_source_global_id

  def user_password(), do: @user_password

  def create_air_user() do
    admin_group = Repo.one!(from group in Group, where: group.name == @admin_group_name)

    # create user
    %User{}
    |> User.new_user_changeset(%{
          email: "user_#{:erlang.unique_integer([:positive])}@aircloak.com",
          name: "user_#{:erlang.unique_integer([:positive])}",
          password: @user_password,
          password_confirmation: @user_password,
          groups: [admin_group.id]
        })
    |> Repo.insert!()
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp await_data_source() do
    if Repo.one(from(ds in DataSource, where: ds.global_id == @data_source_global_id)) == nil do
      :timer.sleep(100)
      await_data_source()
    end
  end

  defp setup_cloak_database() do
    Cloak.Test.DB.start_link()
    :ok = Cloak.Test.DB.create_table("users", "name TEXT, height INTEGER")
    :ok = insert_rows(1..100, "users", ["name", "height"], ["john", 180])
  end

  defp setup_data_source() do
    # delete previous entries
    Repo.delete_all(View)
    Repo.delete_all("data_sources_groups")
    Repo.delete_all("groups_users")
    Repo.delete_all(User)
    Repo.delete_all(Group)

    # create group
    admin_group =
      %Group{}
      |> Group.changeset(%{name: @admin_group_name, admin: true})
      |> Repo.insert!()

    # connect data source to group
    from(ds in DataSource, where: ds.global_id == @data_source_global_id)
    |> Repo.one!()
    |> Repo.preload([:groups])
    |> DataSource.changeset(%{groups: [admin_group.id]})
    |> Repo.update!()
  end

  defp insert_rows(user_id_range, table, columns, values) do
    Cloak.Test.DB.add_users_data(table, columns, Enum.map(user_id_range, &["user#{&1}" | values]))
  end
end
