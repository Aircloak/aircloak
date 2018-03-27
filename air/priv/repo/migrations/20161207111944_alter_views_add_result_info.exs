defmodule Air.Repo.Migrations.AlterViewsAddResultInfo do
  use Ecto.Migration

  def change do
    alter table(:views) do
      add(:result_info, :map, default: fragment("'{}'::jsonb"), null: false)
    end
  end
end
