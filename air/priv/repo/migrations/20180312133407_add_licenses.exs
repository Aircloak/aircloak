defmodule Air.Repo.Migrations.AddLicenses do
  use Ecto.Migration

  def change do
    create table(:licenses) do
      add :text, :text

      timestamps()
    end
  end
end
