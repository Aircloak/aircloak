defmodule Air.Repo.Migrations.AddChangeToPrivacyPolicy do
  use Ecto.Migration

  def change do
    alter table(:privacy_policies) do
      add(:changes, :text)
    end
  end
end
