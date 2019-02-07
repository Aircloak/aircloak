defmodule Air.Repo.Migrations.CheckUniqueViewsAndTables do
  use Ecto.Migration

  def up do
    execute("create type selectable_type as enum ('view', 'analyst_table')")

    create table(:user_selectables) do
      add(:user_id, references(:users, on_delete: :delete_all), null: false)
      add(:data_source_id, references(:data_sources, on_delete: :delete_all), null: false)
      add(:name, :text, null: false)
      add(:sql, :text, null: false)
      add(:result_info, :map, default: fragment("'{}'::jsonb"))
      add(:broken, :boolean)
      add(:type, :selectable_type)
    end

    create(unique_index(:user_selectables, [:user_id, :data_source_id, :name]))

    # views
    execute("""
      insert into user_selectables(user_id, data_source_id, name, sql, result_info, broken, type)
        select user_id, data_source_id, name, sql, result_info, broken, 'view' from views
    """)

    execute("drop table views")
    execute("create view views as select * from user_selectables where type = 'view'")
    execute("alter view views alter column type set default 'view'")

    # analyst tables
    execute("""
      insert into user_selectables(user_id, data_source_id, name, sql, result_info, broken, type)
        select user_id, data_source_id, name, sql, result_info, false, 'analyst_table' from analyst_tables
    """)

    execute("drop table analyst_tables")
    execute("create view analyst_tables as select * from user_selectables where type = 'analyst_table'")
    execute("alter view analyst_tables alter column type set default 'analyst_table'")
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
        select user_id, data_source_id, name, sql, result_info, broken from user_selectables where type='view'
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
        select user_id, data_source_id, name, sql, result_info from user_selectables where type='analyst_table'
    """)

    drop(table(:user_selectables))
    execute("drop type selectable_type")
  end
end
