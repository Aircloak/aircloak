defmodule Air.Repo.Migrations.AddErrorsToDataSources do
  use Ecto.Migration

  def change do
    alter table(:data_sources) do
      add(:errors, :string, default: "")
    end
  end
end
