defmodule Air.Repo.Migrations.AddIndexToAuditLogs do
  use Ecto.Migration

  def change do
    create(index(:audit_logs, [:inserted_at], using: :btree))
  end
end
