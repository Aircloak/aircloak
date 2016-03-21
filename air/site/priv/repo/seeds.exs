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

alias Air.User
alias Air.Organisation

# aircloak organisation
aircloak_organisation =
  %Organisation{}
  |> Organisation.changeset(%{name: "Aircloak test organisation"})
  |> Air.Repo.insert!

# admin user
aircloak_organisation
|> Ecto.build_assoc(:users)
|> User.changeset(%{
      email: "admin@aircloak.com",
      password: "1234",
      password_confirmation: "1234",
      name: "Aircloak test user"
    })
|> Air.Repo.insert!
