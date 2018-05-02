defmodule Air.Repo.Migrations.AddUserIdToAuditLogs do
  use Ecto.Migration

  def up do
    execute("TRUNCATE TABLE audit_logs")

    alter table(:audit_logs) do
      remove(:user)
      add(:user_id, references(:users, on_delete: :delete_all))
    end
  end
end
