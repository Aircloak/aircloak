defmodule Air.Repo.Migrations.MarkAllExistingTasksAsPermanent do
  use Ecto.Migration

  def up do
    Air.Repo.update_all(Air.Task, set: [permanent: true])
  end

  def down do
    Air.Repo.update_all(Air.Task, set: [permanent: false])
  end
end
