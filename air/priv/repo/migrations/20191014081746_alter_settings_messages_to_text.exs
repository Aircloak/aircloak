defmodule Air.Repo.Migrations.AlterSettingsMessagesToText do
  use Ecto.Migration

  def up do
    alter table(:settings) do
      modify(:login_message, :text)
      modify(:main_message, :text)
    end
  end

  def down do
    alter table(:settings) do
      modify(:login_message, :string)
      modify(:main_message, :string)
    end
  end
end
