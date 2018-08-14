defmodule Aircloak.ChildSpec do
  @moduledoc "Helpers for specifying supervisor children."

  @type child_spec :: Supervisor.child_spec() | module | {module, any}

  @doc "Specifies a supervisor child powered by an arbitrary module."
  @spec supervisor(module, atom, [any], Keyword.t()) :: Supervisor.child_spec()
  def supervisor(module, function, args, overrides \\ []),
    do:
      Supervisor.child_spec(
        %{
          id: module,
          restart: :permanent,
          shutdown: :infinity,
          type: :supervisor,
          start: {module, function, args}
        },
        overrides
      )

  @doc "Specifies a child powered by the `Supervisor` module."
  @spec supervisor([child_spec], [Supervisor.option()]) :: Supervisor.child_spec()
  def supervisor(children, supervisor_options),
    do:
      supervisor(
        Supervisor,
        :start_link,
        [children, supervisor_options],
        id: Keyword.get(supervisor_options, :name, Supervisor)
      )

  @doc "Specifies a child powered by the `DynamicSupervisor` module."
  @spec dynamic_supervisor([DynamicSupervisor.option()]) :: Supervisor.child_spec()
  def dynamic_supervisor(supervisor_options \\ []) do
    default_options = [
      strategy: :one_for_one,
      id: Keyword.get(supervisor_options, :name, DynamicSupervisor)
    ]

    Supervisor.child_spec({DynamicSupervisor, Keyword.merge(default_options, supervisor_options)}, [])
  end

  @doc "Specifies a child powered by the `Task.Supervisor` module."
  @spec task_supervisor([Task.Supervisor.option()]) :: Supervisor.child_spec()
  def task_supervisor(task_supervisor_options),
    do:
      supervisor(
        Task.Supervisor,
        :start_link,
        [task_supervisor_options],
        id: Keyword.get(task_supervisor_options, :name, Task.Supervisor)
      )

  @doc "Specifies a child powered by the `GenServer` module."
  @spec gen_server(module, any, GenServer.options(), Keyword.t()) :: Supervisor.child_spec()
  def gen_server(module, arg, gen_server_options \\ [], overrides \\ []),
    do:
      Supervisor.child_spec(
        %{
          id: module,
          restart: :permanent,
          shutdown: 5000,
          type: :worker,
          start: {GenServer, :start_link, [module, arg, gen_server_options]}
        },
        overrides
      )

  @doc "Specifies a child powered by the `Registry` module."
  @spec registry(Registry.keys(), Registry.registry(), Keyword.t()) :: Supervisor.child_spec()
  def registry(keys, name, registry_options \\ []),
    do:
      Supervisor.child_spec(
        {Registry, Keyword.merge([keys: keys, name: name], registry_options)},
        id: name
      )

  @doc "Specifies a child powered by the `Task` module."
  @spec task((() -> any), Keyword.t()) :: Supervisor.child_spec()
  def task(fun, overrides \\ []),
    do:
      Supervisor.child_spec(
        %{id: Task, start: {Task, :start_link, [fun]}, shutdown: :brutal_kill},
        overrides
      )

  @doc "Specifies a child powered by the `Agent` module."
  @spec agent((() -> any), Keyword.t()) :: Supervisor.child_spec()
  def agent(fun, agent_options \\ [], overrides \\ []),
    do:
      Supervisor.child_spec(
        %{id: Agent, type: :worker, start: {Agent, :start_link, [fun, agent_options]}},
        overrides
      )

  defmodule Supervisor do
    @moduledoc """
    Simplifies the `child_spec/2` definition for a supervisor module.

    Inside the module which starts a supervisor, you can `use` this module to define a `child_spec/2`.
    For example:

    ```
    defmodule MyRepo do
      use Ecto.Repo, otp_app: :my_app
      use Aircloak.ChildSpec.Supervisor
      # ...
    end
    ```
    """

    @doc false
    defmacro __using__(overrides) do
      quote bind_quoted: [overrides: Macro.escape(overrides)] do
        @doc false
        def child_spec(_arg), do: Aircloak.ChildSpec.supervisor(__MODULE__, :start_link, [], unquote(overrides))
      end
    end
  end

  defmodule Task do
    @moduledoc """
    Simplifies the `child_spec/2` definition for a task module.

    Inside the module which starts a task, you can `use` this module to define a `child_spec/2`.
    For example:

    ```
    defmodule MyTask do
      use Aircloak.ChildSpec.Task
      # ...
    end
    ```
    """

    @doc false
    defmacro __using__(options) do
      quote bind_quoted: [options: options || []] do
        @doc false
        def child_spec(_arg),
          do:
            Elixir.Supervisor.Spec.worker(
              __MODULE__,
              Keyword.get(unquote(options), :args, []),
              restart: Keyword.get(unquote(options), :restart, :permanent)
            )
      end
    end
  end
end
