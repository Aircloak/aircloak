defmodule Air.Repo.Migrations.CreateLogs do
  use Ecto.Migration
  alias Air.Schemas.Log

  def change do
    Log.Source.create_type()

    create table(:logs) do
      add(:timestamp, :naive_datetime_usec, null: false)
      add(:hostname, :string, null: false)
      add(:source, Log.Source.type(), null: false)
      add(:message, :text, null: false)
    end

    create(index(:logs, [:timestamp, :source]))
  end
end
