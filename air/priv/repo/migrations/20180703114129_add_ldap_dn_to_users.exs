defmodule Air.Repo.Migrations.AddLdapDnToUsers do
  use Ecto.Migration

  def change do
    alter table(:users) do
      add(:ldap_dn, :string)
    end

    create(unique_index(:users, [:ldap_dn]))
  end
end
