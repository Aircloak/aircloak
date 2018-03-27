defmodule Air.Repo.Migrations.AddAuditLogToggleToSettings do
  use Ecto.Migration

  def change do
    alter table(:settings) do
      add(:audit_log_enabled, :boolean, default: true)
    end
  end
end
