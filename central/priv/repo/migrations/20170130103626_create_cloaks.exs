defmodule Central.Repo.Migrations.CreateCloaks do
  use Ecto.Migration

  def change do
    create table(:cloaks) do
      add(:name, :string, null: false)
      add(:air_id, references(:airs, on_delete: :delete_all), null: false)
      add(:status, :integer, null: false)
      timestamps()
    end

    create(unique_index(:cloaks, [:name, :air_id]))
  end
end
