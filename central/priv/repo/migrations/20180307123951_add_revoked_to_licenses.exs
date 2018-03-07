defmodule Central.Repo.Migrations.AddRevokedToLicenses do
  use Ecto.Migration

  def change do
    alter table(:licenses) do
      add :revoked, :boolean
    end
  end
end
