defmodule Air.Repo.Migrations.CheckUniqueViewsAndTables do
  use Ecto.Migration

  def up do
    create table(:user_selectables) do
      add(:user_id, references(:users, on_delete: :delete_all), null: false)
      add(:data_source_id, references(:data_sources, on_delete: :delete_all), null: false)
      add(:name, :text, null: false)
      add(:sql, :text, null: false)
      add(:result_info, :map, default: fragment("'{}'::jsonb"))
      add(:broken, :boolean)
      add(:type, :integer)
    end

    create(unique_index(:user_selectables, [:user_id, :data_source_id, :name]))

    # views
    execute("""
      insert into user_selectables(user_id, data_source_id, name, sql, result_info, broken, type)
        select user_id, data_source_id, name, sql, result_info, broken, 0 from views
    """)

    execute("drop table views")
    execute("create view views as select * from user_selectables where type = 0")
    execute("alter view views alter column type set default 0")

    # analyst tables
    execute("""
      insert into user_selectables(user_id, data_source_id, name, sql, result_info, broken, type)
        select user_id, data_source_id, name, sql, result_info, null, 1 from analyst_tables
    """)

    execute("drop table analyst_tables")
    execute("create view analyst_tables as select * from user_selectables where type = 1")
    execute("alter view analyst_tables alter column type set default 1")
  end

  def down do
    # views
    execute("drop view views")

    create table(:views) do
      add(:user_id, references(:users, on_delete: :delete_all), null: false)
      add(:data_source_id, references(:data_sources, on_delete: :delete_all), null: false)
      add(:name, :text, null: false)
      add(:sql, :text, null: false)
      add(:result_info, :map, default: fragment("'{}'::jsonb"))
      add(:broken, :boolean)
    end

    create(unique_index(:views, [:user_id, :data_source_id, :name]))
    create(index(:views, [:user_id]))
    create(index(:views, [:data_source_id]))

    execute("""
      insert into views(user_id, data_source_id, name, sql, result_info, broken)
        select user_id, data_source_id, name, sql, result_info, broken from user_selectables where type=0
    """)

    # analyst_tables

    execute("drop view analyst_tables")

    create table(:analyst_tables) do
      add(:user_id, references(:users, on_delete: :delete_all), null: false)
      add(:data_source_id, references(:data_sources, on_delete: :delete_all), null: false)
      add(:name, :text, null: false)
      add(:sql, :text, null: false)
      add(:result_info, :map, default: fragment("'{}'::jsonb"))
    end

    create(unique_index(:analyst_tables, [:user_id, :data_source_id, :name]))

    execute("""
      insert into analyst_tables(user_id, data_source_id, name, sql, result_info)
        select user_id, data_source_id, name, sql, result_info from user_selectables where type=1
    """)

    drop(table(:user_selectables))
  end
end
