defmodule Air.Repo.Migrations.AddCloakIdToQueries do
  use Ecto.Migration

  def change do
    alter table(:queries) do
      add(:cloak_id, :string)
    end
  end
end
