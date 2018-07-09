defmodule Air.Repo.Migrations.AddLdapEnabledSetting do
  use Ecto.Migration

  def change do
    alter table(:settings) do
      add(:ldap_enabled, :boolean, null: false, default: false)
    end
  end
end
