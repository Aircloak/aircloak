defmodule Air.Repo.Migrations.CreateAuditLog do
  use Ecto.Migration

  def change do
    create table(:audit_logs) do
      add(:event, :string)
      add(:user, :string)
      add(:metadata, :text)

      timestamps(type: :naive_datetime_usec)
    end
  end
end
