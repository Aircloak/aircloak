defmodule Central.Repo.Migrations.AlterCloaksChangeDatasources do
  use Ecto.Migration

  def change do
    alter table(:cloaks) do
      remove :data_sources
      add :data_source_names, {:array, :string}, null: false, default: []
    end
  end
end
