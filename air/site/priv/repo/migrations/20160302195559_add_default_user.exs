defmodule Air.Repo.Migrations.AddDefaultUser do
  use Ecto.Migration

  def change do
    alias Air.User
    user_params = %{
      "email" => "admin@aircloak.com",
      "password" => "1234",
      "password_confirmation" => "1234",
      "name" => "Aircloak test user"
    }
    changeset = User.changeset(%User{}, user_params)
    Air.Repo.insert(changeset)
  end
end
