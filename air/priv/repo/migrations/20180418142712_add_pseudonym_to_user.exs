defmodule Air.Repo.Migrations.AddPseudonymToUser do
  use Ecto.Migration

  def change do
    alter table(:users) do
      add(:pseudonym, :text, default: nil)
    end
  end
end
