defmodule AircloakCI.ImageCleaner do
  @moduledoc """
  Periodic cleanup of obsolete git image tags.

  Every docker image is tagged with `git_sha_xyz`. This module periodically gathers git ids for known heads, and removes
  all `git_sha_xyz` tags which are related to unknown heads.
  """

  require Logger
  require Aircloak
  alias AircloakCI.CmdRunner

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp cleanup_old_images() do
    AircloakCI.CmdRunner.Supervisor.lock_start(fn ->
      if AircloakCI.CmdRunner.Supervisor.job_count() == 0 do
        known_shas = compute_known_shas()

        existing_sha_tagged_images()
        |> Stream.reject(&MapSet.member?(known_shas, &1.sha))
        |> Enum.each(&remove_docker_image/1)
      end
    end)
  end

  defp remove_docker_image(descriptor) do
    full_image_name = "#{descriptor.image}:#{descriptor.tag}"

    case CmdRunner.run_with_output("docker rmi #{full_image_name}", lock_start?: false) do
      {:ok, _success} -> Logger.info("removed docker image #{full_image_name}")
      {:error, error} -> Logger.error("error removing docker image #{full_image_name}:\n#{error}")
    end
  end

  defp existing_sha_tagged_images() do
    ~s/docker images | grep aircloak | awk '{print $1 " " $2}' | grep 'git_sha_'/
    |> CmdRunner.run_with_output!(lock_start?: false)
    |> String.split("\n")
    |> Stream.reject(&(&1 == ""))
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
    |> CmdRunner.run_with_output!(cd: AircloakCI.LocalProject.master_src_folder(), lock_start?: false)
    |> String.split("\n")
    |> Stream.reject(&(&1 == ""))
    |> MapSet.new()
  end

  # -------------------------------------------------------------------
  # Supervision tree
  # -------------------------------------------------------------------

  @doc false
  def child_spec(_) do
    Periodic.child_spec(
      id: __MODULE__,
      run: fn -> cleanup_old_images() end,
      every: :timer.minutes(1),
      initial_delay: Aircloak.in_env(test: :infinity, else: 0),
      overlap?: false,
      timeout: :timer.seconds(30)
    )
  end
end
