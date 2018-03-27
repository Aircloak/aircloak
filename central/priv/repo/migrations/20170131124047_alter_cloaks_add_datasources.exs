defmodule Central.Repo.Migrations.AlterCloaksAddDatasources do
  use Ecto.Migration

  def change do
    alter table(:cloaks) do
      add(:data_sources, :integer, null: false, default: 0)
    end
  end
end
