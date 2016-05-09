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

# admin user
admin_organisation = Air.Repo.get_by!(Organisation, name: Organisation.admin_group_name())

admin_organisation
|> Ecto.build_assoc(:users)
|> User.changeset(%{
  email: "admin@aircloak.com",
  password: "1234",
  password_confirmation: "1234",
  name: "Aircloak test administrator",
  role_id: User.role_id(:org_admin)
})
|> Air.Repo.insert!


# test client organisation
client_organisation = %Organisation{}
|> Organisation.changeset(%{name: "Client test organisation"})
|> Air.Repo.insert!

# org admin
client_organisation
|> Ecto.build_assoc(:users)
|> User.changeset(%{
  email: "org_admin@aircloak.com",
  password: "1234",
  password_confirmation: "1234",
  name: "Test client org admin",
  role_id: User.role_id(:org_admin)
})
|> Air.Repo.insert!

# plain user
client_organisation
|> Ecto.build_assoc(:users)
|> User.changeset(%{
  email: "user@aircloak.com",
  password: "1234",
  password_confirmation: "1234",
  name: "Test client regular user",
  role_id: User.role_id(:user)
})
|> Air.Repo.insert!
