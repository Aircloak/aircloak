defmodule Air.Repo.Migrations.AddCreationStatusToAnalystTable do
  use Ecto.Migration
  require EctoEnum

  EctoEnum.defenum(CreationStatus, :creation_status, [:pending, :succeeded, :failed])

  def up do
    CreationStatus.create_type()

    alter table(:user_selectables) do
      add(:creation_status, :creation_status, null: false, default: "succeeded")
    end

    execute("drop view analyst_tables")
    execute("create view analyst_tables as select * from user_selectables where type = 'analyst_table'")
    execute("alter view analyst_tables alter column type set default 'analyst_table'")
    execute("alter view analyst_tables alter column creation_status set default 'pending'")

    execute("drop view views")
    execute("create view views as select * from user_selectables where type = 'view'")
    execute("alter view views alter column type set default 'view'")
    execute("alter view views alter column creation_status set default 'succeeded'")
  end

  def down do
    alter table(:user_selectables) do
      remove(:creation_status)
    end

    CreationStatus.drop_type()

    execute("drop view analyst_tables")
    execute("create view analyst_tables as select * from user_selectables where type = 'analyst_table'")
    execute("alter view analyst_tables alter column type set default 'analyst_table'")

    execute("drop view views")
    execute("create view views as select * from user_selectables where type = 'view'")
    execute("alter view views alter column type set default 'view'")
  end
end
