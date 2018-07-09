defmodule Air.Repo.Migrations.AddLdapSettings do
  use Ecto.Migration

  def change do
    alter table(:settings) do
      add(:ldap_host, :string)
      add(:ldap_port, :integer)
      add(:ldap_ssl, :boolean, null: false, default: false)
      add(:ldap_ca_cert, :string)
    end
  end
end
