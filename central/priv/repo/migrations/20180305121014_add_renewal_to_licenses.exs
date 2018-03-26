defmodule Central.Repo.Migrations.AddRenewalToLicenses do
  use Ecto.Migration

  def change do
    alter table(:licenses) do
      add(:auto_renew, :boolean)
      add(:length_in_days, :integer)
    end
  end
end
