defmodule AircloakCI.Queue do
  @moduledoc "Queueing helpers."

  require Logger

  @type id :: :docker_build | :compile | :test | :compliance | :github_api | :job | :start_command

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Sets up the CI queues."
  @spec create_queues() :: :ok
  def create_queues() do
    for {id, spec} <- Application.fetch_env!(:aircloak_ci, :queues), do: :jobs.add_queue(id, spec)
    :ok
  end

  @doc """
  Waits in the given queue, executes the function when approved, and returns the function result.

  The queue will be created if needed.
  """
  @spec exec(id, (() -> result)) :: result when result: var
  def exec(id, fun) do
    {:ok, ref} = :jobs.ask(id)

    try do
      fun.()
    after
      :jobs.done(ref)
    end
  end
end
