defmodule Central.Repo.Migrations.AddVersionToAirAndCloak do
  use Ecto.Migration

  def change do
    alter table(:airs) do
      add(:version, :string)
    end

    alter table(:cloaks) do
      add(:version, :string)
    end
  end
end
