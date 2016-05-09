defmodule Cloak.TaskCoordinatorSupervisor do
  def start_link do
    import  Supervisor.Spec

    children = [worker(:task_coordinator, [], restart: :temporary)]

    Supervisor.start_link(children, strategy: :simple_one_for_one, name: __MODULE__)
  end

  def start_worker(args) do
    Supervisor.start_child(__MODULE__, [args, []])
  end
end
