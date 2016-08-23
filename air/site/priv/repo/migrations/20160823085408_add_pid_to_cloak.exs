defmodule Air.Repo.Migrations.AddPidToCloak do
  use Ecto.Migration

  def change do
    alter table(:cloaks) do
      add :raw_pid, :string
    end
  end
end
