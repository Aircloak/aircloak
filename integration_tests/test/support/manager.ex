defmodule IntegrationTest.Manager do
  import Ecto.Query, only: [from: 2]

  alias Air.Repo
  alias Air.Schemas.{DataSource, Group, User, View}

  @admin_group_name "admins"
  @user_mail "integration_test@aircloak.com"
  @user_password "1234"
  @data_source_global_id "postgres/cloaktest1-native@localhost"


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  def setup() do
    setup_cloak_database()
    setup_air_user()
  end

  def data_source_global_id(), do: @data_source_global_id

  def user_mail(), do: @user_mail

  def user_password(), do: @user_password

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

    # create user
    %User{}
    |> User.new_user_changeset(%{
          email: @user_mail,
          name: "integration_test",
          password: @user_password,
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
