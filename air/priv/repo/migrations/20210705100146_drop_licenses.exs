defmodule Air.Repo.Migrations.DropLicenses do
  use Ecto.Migration

  def up do
    drop(table(:licenses))
  end

  def down do
    create table(:licenses) do
      add(:text, :text)
      timestamps(type: :naive_datetime_usec)
    end
  end
end
