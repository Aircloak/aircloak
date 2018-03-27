# Script for populating the development/test database.
#
# Inside the script, you can read and write to any of your
# repositories directly:
#
#     Air.Repo.insert!(%Air.SomeModel{})
#
# We recommend using the bang functions (`insert!`, `update!`
# and so on) as they will fail if something goes wrong.
#
# Also, we don't recommend using delete/update operations here. This script
# is meant to be forward only. If existing rows have to be changed, for example
# if the database structure changes, it's advised to recreate the entire database.

alias Air.Service.{User, License}

# admin user
admin_group =
  case User.admin_groups() do
    [] ->
      User.create_group!(%{
        name: "admin",
        admin: true
      })

    [group | _] ->
      group
  end

User.create!(%{
  email: "admin@aircloak.com",
  password: "1234",
  password_confirmation: "1234",
  name: "Aircloak test administrator",
  groups: [admin_group.id]
})

# plain user
User.create!(%{
  email: "user@aircloak.com",
  password: "1234",
  password_confirmation: "1234",
  name: "Test client regular user"
})

:ok = License.load(File.read!("priv/dev_license.lic"))
