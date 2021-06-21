defmodule Air.Repo.Migrations.AddSystemUser do
  use Ecto.Migration

  alias Air.Service.User

  def up, do:
    User.create!(%{name: "System user", login: "system_user", system: true})

  def down do
    user = User.system_user!("system_user")
    Air.Repo.delete!(user)
  end
end
