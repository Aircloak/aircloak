defmodule Air.Repo.Migrations.AlterDataSourcesErrors do
  use Ecto.Migration

  def up() do
    alter table(:data_sources) do
      modify(:errors, :text, default: "")
    end
  end

  def down() do
    # Clearing errors, since they might be longer than 255 characters.
    Ecto.Adapters.SQL.query!(Air.Repo, "update data_sources set errors=$1", [""])

    alter table(:data_sources) do
      modify(:errors, :string, default: "")
    end
  end
end
