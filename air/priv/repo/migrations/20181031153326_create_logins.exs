defmodule Air.Repo.Migrations.CreateLogins do
  use Ecto.Migration

  import EctoEnum
  defenum(LoginType, :login_type, [:main, :psql])

  def up do
    LoginType.create_type()

    create table(:logins) do
      add(:user_id, references(:users, on_delete: :delete_all))
      add(:login, :string, null: false)
      add(:login_type, :login_type, null: false)
      add(:hashed_password, :string, null: false)
      add(:ldap_dn, :string)

      timestamps()
    end

    create(unique_index(:logins, [:login]))
    create(unique_index(:logins, [:ldap_dn]))

    execute("""
      CREATE UNIQUE INDEX logins_user_id_login_type_index ON logins (user_id, login_type)
      WHERE login_type = 'main'
    """)

    execute("""
      INSERT INTO logins (user_id, login, hashed_password, inserted_at, updated_at, login_type, ldap_dn)
      SELECT id, login, hashed_password, inserted_at, updated_at, 'main', ldap_dn
      FROM users
    """)

    drop(unique_index(:users, [:login]))
    drop(unique_index(:users, [:ldap_dn]))

    alter table(:users) do
      remove(:login)
      remove(:hashed_password)
      remove(:ldap_dn)
    end
  end

  def down do
    alter table(:users) do
      add(:login, :string)
      add(:hashed_password, :string)
      add(:ldap_dn, :string)
    end

    create(unique_index(:users, [:login]))
    create(unique_index(:users, [:ldap_dn]))

    execute("""
      UPDATE users
      SET login = logins.login, hashed_password = logins.hashed_password, ldap_dn = logins.ldap_dn
      FROM logins
      WHERE users.id = logins.user_id AND logins.login_type = 'main'
    """)

    execute("DROP INDEX logins_user_id_login_type_index")

    drop(unique_index(:logins, [:login]))
    drop(unique_index(:logins, [:ldap_dn]))

    drop(table(:logins))

    LoginType.drop_type()
  end
end
