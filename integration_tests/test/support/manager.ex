defmodule IntegrationTest.Manager do
  import Ecto.Query, only: [from: 2]

  alias Air.{DataSource, Group, Repo, User}

  @admin_group_name "admins"
  @user_mail "integration_test@aircloak.com"
  @data_source_global_id "postgres/cloaktest1-native@localhost"


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  def setup() do
    setup_cloak_database()
    setup_air_user()
  end

  def data_source_global_id(), do:
    {:global_id, @data_source_global_id}

  def air_user(), do:
    Repo.one!(from u in User, where: u.email == @user_mail)


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp setup_cloak_database() do
    Cloak.Test.DB.start_link()
    :ok = Cloak.Test.DB.create_table("users", "name TEXT, height INTEGER")
    :ok = insert_rows(1..100, "users", ["name", "height"], ["john", 180])
  end

  defp setup_air_user() do
    # delete previous entries
    Repo.delete_all(
      from dg in "data_sources_groups",
      join: g in Group, on: g.id == dg.group_id,
      where: g.name == @admin_group_name
    )
    Repo.delete_all(
      from gu in "groups_users",
      join: g in Group, on: g.id == gu.group_id,
      where: g.name == @admin_group_name
    )
    Repo.delete_all(from u in User, where: u.email == @user_mail)
    Repo.delete_all(from g in Group, where: g.name == @admin_group_name)

    # create group
    admin_group =
      %Group{}
      |> Group.changeset(%{name: @admin_group_name, admin: true})
      |> Repo.insert!()

    # create user
    %User{}
    |> User.new_user_changeset(%{
          email: @user_mail,
          name: "integration_test",
          password: "1234",
          password_confirmation: "1234",
          groups: [admin_group.id]
        })
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
