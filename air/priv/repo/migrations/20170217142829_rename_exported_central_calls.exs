defmodule Air.Repo.Migrations.RenameExportedCentralCalls do
  use Ecto.Migration

  def change do
    rename(table(:exported_central_calls), to: table(:exports_for_aircloak))
  end
end
