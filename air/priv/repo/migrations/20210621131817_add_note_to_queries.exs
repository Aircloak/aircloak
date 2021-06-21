defmodule Air.Repo.Migrations.AddNoteToQueries do
  use Ecto.Migration

  def change do
    alter table(:queries) do
      add(:note, :text)
    end
  end
end
