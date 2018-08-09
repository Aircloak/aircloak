defmodule Air.Repo.Migrations.AddLdapDnToGroups do
  use Ecto.Migration

  def change do
    alter table(:groups) do
      add(:ldap_dn, :string)
    end

    create(unique_index(:groups, [:ldap_dn]))
  end
end
