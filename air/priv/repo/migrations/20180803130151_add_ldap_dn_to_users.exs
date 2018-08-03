defmodule Air.Repo.Migrations.AddLdapDnToUsers do
  use Ecto.Migration

  def change do
    alter table(:users) do
      add(:ldap_dn, :string)
    end
  end
end
