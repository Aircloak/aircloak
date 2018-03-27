defmodule Air.Repo.Migrations.AddRowsToQuery do
  use Ecto.Migration

  def change do
    alter table(:queries) do
      add(:rows, :binary)
    end
  end
end
