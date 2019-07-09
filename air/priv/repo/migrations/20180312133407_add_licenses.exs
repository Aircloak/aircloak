defmodule Air.Repo.Migrations.AddLicenses do
  use Ecto.Migration

  def change do
    create table(:licenses) do
      add(:text, :text)

      timestamps(type: :naive_datetime_usec)
    end
  end
end
