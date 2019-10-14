defmodule Air.Repo.Migrations.AddMessagesToSettings do
  use Ecto.Migration

  def change do
    alter table(:settings) do
      add(:login_message, :string)
      add(:main_message, :string)
    end
  end
end
