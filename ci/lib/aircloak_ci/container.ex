defmodule AircloakCI.Container do
  @moduledoc """
  Functions for working with docker containers.

  This module allows us to start uniquely named containers, and execute commands on them.

  The actual docker manipulation is done with the help of the shell script. For example, see `cloak/ci/container.sh`.
  This script has to expose a well defined interface for operations such as image building, container starting, and
  execution of commands in the container.

  The containers are associated with the process which created them. If the process dies, the containers will be
  dropped. If the CI server dies, the containers will be left dangling (and possibly running). However, when the CI
  server is restarted, it will remove all dangling containers and networks.

  The module also understands the relationship of "children". Every named container can have associated containers,
  starting with the same name. For example, for a container `foo`, we can also have `foo_bar`, and `foo_baz` containers
  running. These are considered to be child containers of `foo`. When stopping `foo`, these child containers will be
  stopped as well. As long as the owner process of the `foo` container is running, `foo_bar` and `foo_baz` are not
  considered to be dangling.
  """

  alias Aircloak.ChildSpec
  alias AircloakCI.CmdRunner

  @type t :: %{name: String.t, script: String.t, log_file: String.t}


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Returns true if the image for the container is built."
  @spec built?(String.t) :: boolean
  def built?(script), do:
    os_cmd("#{script} is_image_built") == ["yes"]

  @doc "Builds the container image."
  @spec build(String.t, String.t) :: :ok | {:error, String.t}
  def build(script, log_file), do:
    invoke_script(script, "build_image", log_file, timeout: :timer.hours(1))

  @doc "Starts the container and logs the output to the provided log file."
  @spec start(String.t, String.t) :: {:ok, t} | {:error, String.t}
  def start(script, log_file) do
    container = new(script, log_file)
    register(container)
    with :ok <- invoke_script(container, "start_container #{container.name}", timeout: :timer.seconds(30)), do:
      {:ok, container}
  end

  @doc "Stops the container, its child containers, and associated networks."
  @spec stop(String.t) :: :ok
  def stop(container_name) do
    container_name |> associated_containers() |> Enum.each(&remove_container/1)
    container_name |> associated_networks() |> Enum.each(&remove_network/1)
  end

  @doc "Starts the container, executes the provided lambda, stops the container, and returns the lambda result."
  @spec with(String.t, String.t, ((t) -> :ok | {:error, String.t})) :: :ok | {:error, String.t}
  def with(script, log_file, fun) do
    with {:ok, container} <- start(script, log_file) do
      try do
        fun.(container)
      after
        stop(container.name)
      end
    end
  end

  @doc """
  Invokes the container control script.

  The control script is a bash script exposed by components such as cloak, which can be used to build images, start
  containers, and executes some commands in the container.
  """
  @spec invoke_script(t, String.t, CmdRunner.opts) :: :ok | {:error, String.t}
  def invoke_script(container, cmd, opts \\ []), do:
    invoke_script(container.script, cmd, container.log_file, opts)

  @doc "Executes the command sequence in the container, stops on first error."
  @spec exec(t, [String.t], CmdRunner.opts) :: :ok | {:error, String.t}
  def exec(container, commands, opts \\ []) do
    case \
      commands
      |> Stream.map(&(invoke_script(container, "run_in_container #{container.name} #{&1}", opts)))
      |> Stream.drop_while(&match?(:ok, &1))
      |> Enum.take(1)
    do
      [] -> :ok
      [error] -> error
    end
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp new(script, log_file), do:
    %{
      script: script,
      name: "aircloak_ci_" <> Base.url_encode64(:crypto.strong_rand_bytes(16), padding: false),
      log_file: log_file
    }

  defp register(container), do:
    Registry.register(__MODULE__.Registry, :container, container.name)


  # -------------------------------------------------------------------
  # Cleanup of dangling images and networks
  # -------------------------------------------------------------------

  defp cleanup() do
    all_containers() |> dangling() |> Enum.each(&stop/1)
    all_networks() |> dangling() |> Enum.each(&remove_network/1)
    remove_dangling_volumes()

    :timer.sleep(:timer.seconds(10))
    cleanup()
  end

  defp dangling(docker_names), do:
    Enum.reduce(registered_names(), docker_names, &remove_associated(&2, &1))

  defp registered_names(), do:
    Stream.map(Registry.lookup(AircloakCI.Container.Registry, :container), fn({_pid, name}) -> name end)

  defp remove_associated(names, name), do:
    Enum.reject(names, &String.starts_with?(&1, name))

  defp all_containers(), do: associated_containers("aircloak_ci")

  defp all_networks(), do: associated_networks("aircloak_ci")

  defp associated_containers(container_name), do:
    associated_docker_objects(container_name, "docker ps --format='{{.Names}}'")

  defp associated_networks(container_name), do:
    associated_docker_objects(container_name, "docker network ls --format='{{.Name}}'")

  defp associated_docker_objects(container_name, list_cmd), do:
    list_cmd |> os_cmd() |> Enum.filter(&String.starts_with?(&1, container_name))

  defp remove_container(container_name) do
    os_cmd_with_timeout("docker kill #{container_name}")
    os_cmd_with_timeout("docker rm #{container_name}")
  end

  defp remove_network(network_name) do
    network_name
    |> connected()
    |> Enum.each(&os_cmd_with_timeout("docker network disconnect #{network_name} #{&1}"))

    os_cmd_with_timeout("docker network rm #{network_name}")
  end

  defp remove_dangling_volumes(), do:
    "docker volume ls -qf dangling=true"
    |> os_cmd()
    |> Enum.each(&os_cmd_with_timeout("docker volume rm #{&1}"))

  defp connected(network), do:
    os_cmd("docker network inspect #{network} --format '{{range $key, $value := .Containers}} {{println $key}} {{end}}'")


  # -------------------------------------------------------------------
  # Execution of commands
  # -------------------------------------------------------------------

  defp os_cmd_with_timeout(cmd, opts \\ []), do:
    CmdRunner.run(cmd, opts)

  defp os_cmd(cmd), do:
    # Unlike `CmdRunner.run`, `os_cmd` returns the command output.
    # Note that the same could be achieved with `System.cmd`, but `:os.cmd` is more flexible and permissive.
    cmd
    |> to_charlist()
    |> :os.cmd()
    |> to_string()
    |> String.trim()
    |> String.split("\n")
    |> Enum.map(&String.trim/1)

  defp invoke_script(script, cmd, log_file, opts), do:
    # Using CmdRunner here because of its logging and timeout capabilities.
    CmdRunner.run("#{script} #{cmd}", Keyword.merge([logger: CmdRunner.file_logger(log_file)], opts))


  # -------------------------------------------------------------------
  # Supervision tree
  # -------------------------------------------------------------------

  @doc false
  def child_spec(_), do:
    ChildSpec.supervisor(
      [
        ChildSpec.registry(:duplicate, __MODULE__.Registry),
        Supervisor.Spec.worker(Task, [&cleanup/0], id: __MODULE__.Cleanup, shutdown: :brutal_kill)
      ],
      name: __MODULE__,
      strategy: :rest_for_one
    )
end
