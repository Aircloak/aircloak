defmodule Air.Repo.Migrations.RemoveUserToPrivacyPolicyReference do
  use Ecto.Migration

  def up do
    alter table(:users) do
      remove(:accepted_privacy_policy_id)
    end
  end

  def down do
    alter table(:users) do
      add(:accepted_privacy_policy_id, references(:privacy_policies))
    end
  end
end
