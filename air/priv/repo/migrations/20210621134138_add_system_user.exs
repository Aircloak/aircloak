defmodule Air.Repo.Migrations.AddSystemUser do
  use Ecto.Migration

  alias Air.Service.User

  def up, do:
    User.create!(%{name: "System user", login: "system_user", system: true})

  def down do
    {:ok, user} = User.get_by_login("system_user")
    User.delete!(user)
  end
end
