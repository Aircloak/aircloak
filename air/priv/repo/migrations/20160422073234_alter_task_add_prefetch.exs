defmodule Air.Repo.Migrations.AlterTaskAddPrefetch do
  use Ecto.Migration

  def change do
    alter table(:tasks) do
      add(:cloak_id, :string)
      add(:data_source, :string)
      add(:tables, {:array, :string})
    end
  end
end
