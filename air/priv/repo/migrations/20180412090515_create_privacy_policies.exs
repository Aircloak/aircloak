defmodule Air.Repo.Migrations.CreatePrivacyPolicies do
  use Ecto.Migration

  def change do
    create table(:privacy_policies) do
      add :content, :text

      timestamps()
    end

  end
end
