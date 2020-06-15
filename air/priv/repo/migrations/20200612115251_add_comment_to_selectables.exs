defmodule Air.Repo.Migrations.AddCommentToSelectables do
  use Ecto.Migration

  def up do
    execute("drop view analyst_tables")
    execute("drop view views")

    alter table(:user_selectables) do
      add(:comment, :text)
    end

    execute("create view analyst_tables as select * from user_selectables where type = 'analyst_table'")
    execute("alter view analyst_tables alter column type set default 'analyst_table'")
    execute("alter view analyst_tables alter column creation_status set default 'pending'")

    execute("create view views as select * from user_selectables where type = 'view'")
    execute("alter view views alter column type set default 'view'")
    execute("alter view views alter column creation_status set default 'succeeded'")
  end

  def down do
    execute("drop view analyst_tables")
    execute("drop view views")

    alter table(:user_selectables) do
      remove(:comment)
    end

    execute("create view analyst_tables as select * from user_selectables where type = 'analyst_table'")
    execute("alter view analyst_tables alter column type set default 'analyst_table'")
    execute("alter view analyst_tables alter column creation_status set default 'pending'")

    execute("create view views as select * from user_selectables where type = 'view'")
    execute("alter view views alter column type set default 'view'")
    execute("alter view views alter column creation_status set default 'succeeded'")
  end
end
