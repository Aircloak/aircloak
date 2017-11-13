defmodule Mix.Tasks.RecreateDb do
  @shortdoc "Recreates the database from scratch."
  @moduledoc "Recreates the database from scratch."
  use Mix.Task

  # Mix.Task behaviour is not in PLT since Mix is not a runtime dep, so we disable the warning
  @dialyzer :no_undefined_callbacks

  @impl Mix.Task
  def run(_args) do
    Air.Repo.configure()
    Mix.Task.run("ecto.drop")
    Mix.Task.run("ecto.create")
    Mix.Task.run("ecto.migrate")
  end
end
