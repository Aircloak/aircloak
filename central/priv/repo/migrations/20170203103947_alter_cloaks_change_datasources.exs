defmodule Central.Repo.Migrations.AlterCloaksChangeDatasources do
  use Ecto.Migration

  def up() do
    alter table(:cloaks) do
      remove(:data_sources)
      add(:data_source_names, {:array, :string}, null: false, default: [])
    end
  end

  def down() do
    alter table(:cloaks) do
      add(:data_sources, :integer, null: false, default: 0)
      remove(:data_source_names)
    end
  end
end
