defmodule Cloak.TaskCoordinatorSupervisor do
  @moduledoc """
  All task_coordinators are started under this supervisor. In the usual case you should use
  Cloak.TaskCoordinatorSupervisor.start_worker/1 instead of task_coordinator:start_link/2 to make sure newly
  created task_coordinators are part of the supervision tree.
  """

  def start_link do
    import  Supervisor.Spec

    children = [worker(:task_coordinator, [], restart: :temporary)]

    Supervisor.start_link(children, strategy: :simple_one_for_one, name: __MODULE__)
  end

  def start_worker(args) do
    Supervisor.start_child(__MODULE__, [args, []])
  end
end
