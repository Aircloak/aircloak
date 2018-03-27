defmodule Air.Repo.Migrations.AlterQueryAddContext do
  use Ecto.Migration

  require EctoEnum
  EctoEnum.defenum(Context, :query_context, [:http, :psql])

  def up() do
    Context.create_type()

    alter table(:queries) do
      # use default to populate existing rows
      add(:context, :query_context, null: false, default: "http")
    end

    alter table(:queries) do
      # remove the default, so we need to explicitly provide it
      modify(:context, :query_context, default: nil)
    end
  end

  def down() do
    alter table(:queries) do
      remove(:context)
    end

    Context.drop_type()
  end
end
