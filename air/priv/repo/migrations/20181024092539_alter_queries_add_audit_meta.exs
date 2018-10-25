defmodule Air.Repo.Migrations.AlterQueriesAddAuditMeta do
  use Ecto.Migration

  def change do
    alter table(:queries) do
      add(:audit_meta, :map)
    end
  end
end
