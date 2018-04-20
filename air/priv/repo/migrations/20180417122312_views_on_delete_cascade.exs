defmodule Air.Repo.Migrations.ViewsOnDeleteCascade do
  use Ecto.Migration

  def up do
    execute("ALTER TABLE views DROP CONSTRAINT views_user_id_fkey")

    alter table(:views) do
      modify(:user_id, references(:users, on_delete: :delete_all))
    end
  end

  def down do
    execute("ALTER TABLE views DROP CONSTRAINT views_user_id_fkey")

    alter table(:views) do
      modify(:user_id, references(:users), null: false)
    end
  end
end
