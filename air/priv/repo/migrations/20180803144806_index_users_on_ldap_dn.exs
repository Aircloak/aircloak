defmodule Air.Repo.Migrations.IndexUsersOnLdapDn do
  use Ecto.Migration

  def change do
    create(unique_index(:users, [:ldap_dn]))
  end
end
