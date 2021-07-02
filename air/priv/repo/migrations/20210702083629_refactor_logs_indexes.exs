defmodule Air.Repo.Migrations.RefactorLogsIndexes do
  use Ecto.Migration

  def change do
    drop(index(:logs, [:timestamp, :source]))
    create(index(:logs, :timestamp))
  end
end
