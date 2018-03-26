defmodule Air.Repo.Migrations.AddDescriptionToDatasource do
  use Ecto.Migration

  def change do
    alter table(:data_sources) do
      add(:description, :text)
    end
  end
end
