defmodule Air.Repo.Migrations.AddTypeCheckingToSettings do
  use Ecto.Migration

  def change do
    alter table(:settings) do
      add(:type_checking_enabled, :boolean, null: false, default: true)
    end
  end
end
