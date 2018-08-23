defmodule Central.Repo.Migrations.AddFeaturesToLicenses do
  use Ecto.Migration

  def change do
    alter table(:licenses) do
      add(:features, {:array, :string}, null: false, default: [])
    end
  end
end
