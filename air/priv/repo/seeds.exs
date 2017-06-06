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

alias Air.{Schemas.Group, Repo}
import Ecto.Query, only: [from: 2]

# admin user
admin_group = case Repo.all(from g in Group, where: g.admin) do
  [] ->
    %Group{}
    |> Group.changeset(%{
      name: "admin",
      admin: true,
    })
    |> Repo.insert!()
  [group | _] -> group
end

Air.Service.User.create!(%{
  email: "admin@aircloak.com",
  password: "1234",
  password_confirmation: "1234",
  name: "Aircloak test administrator",
  groups: [admin_group.id],
})

# plain user
Air.Service.User.create!(%{
  email: "user@aircloak.com",
  password: "1234",
  password_confirmation: "1234",
  name: "Test client regular user",
})
