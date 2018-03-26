defmodule Air.Repo.Migrations.ConvertAuditLogMetadataType do
  use Ecto.Migration

  def up do
    execute("ALTER TABLE audit_logs ADD COLUMN temp_metadata jsonb;")
    execute("UPDATE audit_logs SET temp_metadata = cast(metadata::text as json)::jsonb;")
    execute("ALTER TABLE audit_logs DROP COLUMN metadata;")
    execute("ALTER TABLE audit_logs RENAME COLUMN temp_metadata TO metadata;")
  end

  def down do
    execute("ALTER TABLE audit_logs ADD COLUMN temp_metadata text;")
    execute("UPDATE audit_logs SET temp_metadata = cast(metadata::json as text);")
    execute("ALTER TABLE audit_logs DROP COLUMN metadata;")
    execute("ALTER TABLE audit_logs RENAME COLUMN temp_metadata TO metadata;")
  end
end
