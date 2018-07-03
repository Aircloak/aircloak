defmodule Air.Repo.Migrations.ConvertLdapCaCertToText do
  use Ecto.Migration

  def up do
    alter table(:settings) do
      modify(:ldap_ca_cert, :text)
    end
  end

  def down do
    alter table(:settings) do
      modify(:ldap_ca_cert, :string)
    end
  end
end
