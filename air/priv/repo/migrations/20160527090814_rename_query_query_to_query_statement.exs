defmodule Air.Repo.Migrations.RenameQueryQueryToQueryStatement do
  use Ecto.Migration

  def change do
    rename(table(:queries), :query, to: :statement)
  end
end
