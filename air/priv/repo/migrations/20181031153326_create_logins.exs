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

      timestamps(type: :naive_datetime_usec)
    end

    create(unique_index(:logins, [:login]))

    execute("""
      CREATE UNIQUE INDEX logins_user_id_login_type_index ON logins (user_id, login_type)
      WHERE login_type = 'main'
    """)

    execute("""
      INSERT INTO logins (user_id, login, hashed_password, inserted_at, updated_at, login_type)
      SELECT id, login, hashed_password, inserted_at, updated_at, 'main'
      FROM users
    """)

    drop(unique_index(:users, [:login]))

    alter table(:users) do
      remove(:login)
      remove(:hashed_password)
    end
  end

  def down do
    alter table(:users) do
      add(:login, :string)
      add(:hashed_password, :string)
    end

    create(unique_index(:users, [:login]))

    execute("""
      UPDATE users
      SET login = logins.login, hashed_password = logins.hashed_password
      FROM logins
      WHERE users.id = logins.user_id AND logins.login_type = 'main'
    """)

    execute("DROP INDEX logins_user_id_login_type_index")

    drop(unique_index(:logins, [:login]))

    drop(table(:logins))

    LoginType.drop_type()
  end
end
