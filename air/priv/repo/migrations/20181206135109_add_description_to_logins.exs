defmodule Air.Repo.Migrations.AddDescriptionToLogins do
  use Ecto.Migration

  def change do
    alter table(:logins) do
      add(:description, :string)
    end
  end
end
