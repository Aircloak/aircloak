defmodule Air.Repo.Migrations.AlterSettingsMessagesToText do
  use Ecto.Migration

  def change do
    alter table(:settings) do
      modify(:login_message, :text)
      modify(:main_message, :text)
    end
  end
end
