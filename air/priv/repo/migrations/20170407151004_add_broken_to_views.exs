defmodule Air.Repo.Migrations.AddBrokenToViews do
  use Ecto.Migration

  def change do
    alter table(:views) do
      add(:broken, :boolean)
    end
  end
end
