defmodule Air.Repo.Migrations.AddUsersCountToQuery do
  use Ecto.Migration

  def change do
    alter table(:queries) do
      add(:users_count, :integer)
    end
  end
end
