# Script for populating the development/test database.
#
# Inside the script, you can read and write to any of your
# repositories directly:
#
#     Central.Repo.insert!(%Central.SomeModel{})
#
# We recommend using the bang functions (`insert!`, `update!`
# and so on) as they will fail if something goes wrong.
#
# Also, we don't recommend using delete/update operations here. This script
# is meant to be forward only. If existing rows have to be changed, for example
# if the database structure changes, it's advised to recreate the entire database.

Central.Service.User.create(%{
  name: "Admin user",
  email: "admin@aircloak.com",
  password: "1234",
  password_confirmation: "1234"
})

Central.Service.Customer.create(%{
  name: "Test customer"
})
