defmodule Air.Repo.Migrations.AddPrivacyPolicyAcceptToUsers do
  use Ecto.Migration

  def change do
    alter table(:users) do
      add(:accepted_privacy_policy_id, references(:privacy_policies))
    end
  end
end
