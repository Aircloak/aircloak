defmodule Air.Repo.Migrations.UserGroupsOnDeleteCascade do
  use Ecto.Migration

  def up do
    execute("ALTER TABLE groups_users DROP CONSTRAINT groups_users_user_id_fkey")

    alter table(:groups_users) do
      modify(:user_id, references(:users, on_delete: :delete_all))
    end
  end
end
