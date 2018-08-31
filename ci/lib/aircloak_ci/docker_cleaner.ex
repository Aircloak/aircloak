defmodule AircloakCI.DockerCleaner do
  @moduledoc """
  Periodic cleanup of dangling docker artifacts, such as images and volumes.

  Every docker image is tagged with `git_sha_xyz`. This module periodically gathers git ids for known heads, and removes
  all `git_sha_xyz` tags which are related to unknown heads.
  """

  require Logger
  require Aircloak
  alias AircloakCI.CmdRunner

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp cleanup() do
    # Since this cleanup removes docker images and volumes, which might cause other docker builds to break, it is
    # running as isolated, while no other external command is running.
    AircloakCI.CmdRunner.Supervisor.lock_start(fn ->
      if AircloakCI.CmdRunner.Supervisor.job_count() == 0 do
        remove_dangling_containers()
        remove_old_sha_tags()
        remove_dangling_images()
        remove_dangling_volumes()
      end
    end)
  end

  defp remove_dangling_containers() do
    "docker ps --quiet -f status=exited"
    |> run_with_output!()
    |> output_lines()
    |> Stream.map(&remove_dangling_container/1)
    |> Stream.filter(&(&1 == :ok))
    |> Enum.count()
    |> case do
      0 -> :ok
      count -> Logger.info("removed #{count} dangling docker containers")
    end
  end

  defp remove_dangling_container(container_id) do
    case run_with_output("docker rm -f #{container_id}") do
      {:ok, _success} ->
        :ok

      {:error, error} ->
        Logger.error("error removing docker container #{container_id}:\n#{error}")
        :error
    end
  end

  defp remove_dangling_volumes() do
    "docker volume ls -qf dangling=true"
    |> run_with_output!()
    |> output_lines()
    |> Stream.map(&remove_docker_volume/1)
    |> Stream.filter(&(&1 == :ok))
    |> Enum.count()
    |> case do
      0 -> :ok
      count -> Logger.info("removed #{count} dangling docker volumes")
    end
  end

  defp remove_docker_volume(volume_name) do
    case run_with_output("docker volume rm #{volume_name}") do
      {:ok, _success} ->
        :ok

      {:error, error} ->
        Logger.error("error removing docker volume #{volume_name}:\n#{error}")
        :error
    end
  end

  defp remove_dangling_images() do
    ~s/docker images --quiet --filter "dangling=true"/
    |> run_with_output!()
    |> output_lines()
    |> Enum.each(&remove_docker_image/1)
  end

  defp remove_docker_image(image_name) do
    case run_with_output("docker rmi #{image_name}") do
      {:ok, _success} ->
        Logger.info("removed docker image #{image_name}")
        :ok

      {:error, error} ->
        Logger.error("error removing docker image #{image_name}:\n#{error}")
        :error
    end
  end

  defp remove_old_sha_tags() do
    known_shas = compute_known_shas()

    existing_sha_tagged_images()
    |> Stream.reject(&MapSet.member?(known_shas, &1.sha))
    |> Enum.each(&remove_docker_image("#{&1.image}:#{&1.tag}"))
  end

  defp existing_sha_tagged_images() do
    ~s/docker images | grep aircloak | awk '{print $1 " " $2}' | grep 'git_sha_'/
    |> run_with_output!()
    |> output_lines()
    |> Stream.map(&String.split/1)
    |> Stream.filter(&match?([_image, "git_sha_" <> _], &1))
    |> Enum.map(fn [image, "git_sha_" <> sha = tag] -> %{image: image, tag: tag, sha: sha} end)
  end

  defp compute_known_shas() do
    [
      # heads of local branches
      ~s/git show-ref --head | awk '{print $1}'/,
      # heads of branches on the remote
      ~s/git ls-remote --heads | awk '{print $1}'/,
      # shas of merge commits on the remote
      ~s/git ls-remote | grep merge | awk '{print $1}'/
    ]
    |> Stream.map(&unique_shas/1)
    |> Enum.reduce(MapSet.new(), &MapSet.union/2)
  end

  defp unique_shas(cmd) do
    ~s/set -eo pipefail; #{cmd}/
    |> run_with_output!(cd: AircloakCI.LocalProject.master_src_folder())
    |> output_lines()
    |> MapSet.new()
  end

  defp run_with_output!(cmd, opts \\ []) do
    {:ok, output} = run_with_output(cmd, opts)
    output
  end

  defp run_with_output(cmd, opts \\ []), do: CmdRunner.run_with_output(cmd, Keyword.merge(opts, lock_start?: false))

  defp output_lines(cmd_output) do
    cmd_output
    |> String.split("\n")
    |> Stream.reject(&(&1 == ""))
    |> Stream.map(&String.trim/1)
  end

  # -------------------------------------------------------------------
  # Supervision tree
  # -------------------------------------------------------------------

  @doc false
  def child_spec(_) do
    Periodic.child_spec(
      id: __MODULE__,
      run: fn -> cleanup() end,
      every: Aircloak.in_env(dev: :timer.seconds(1), else: :timer.minutes(1)),
      initial_delay: Aircloak.in_env(test: :infinity, else: 0),
      overlap?: false,
      timeout: :timer.minutes(5)
    )
  end
end
