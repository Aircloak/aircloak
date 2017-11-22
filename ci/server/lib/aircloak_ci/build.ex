defmodule AircloakCI.Build do
  @moduledoc "Helpers for working with a single build."

  alias AircloakCI.{CmdRunner, Github}
  defstruct [:folder, :repo, :checkout]

  @opaque t :: %__MODULE__{
    folder: String.t,
    repo: String.t,
    checkout: ((t) -> {:ok, t} | {:error, String.t})
  }


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Prepares the build for the given pull request."
  @spec for_pull_request(Github.API.pull_request) :: t
  def for_pull_request(pr), do:
    init_folder(%__MODULE__{
      folder: Path.join(builds_folder(), to_string(pr.number)),
      repo: pr.repo,
      checkout:
        fn(build) ->
          with \
            :ok <- cmd(build, "git fetch --force origin pull/#{pr.number}/merge", timeout: :timer.minutes(1)),
            :ok <- cmd(build, "git checkout #{pr.merge_sha}"),
            do: {:ok, build}
        end
    })

  @doc "Initializes the build."
  @spec initialize(t) :: {:ok, t} | {:error, String.t}
  def initialize(build) do
    with \
      :ok <- clone_repo(build),
      :ok <- cmd(build, "git reset --hard HEAD"),
      do: build.checkout.(build)
  end

  @doc "Executes the given command in the build folder."
  @spec cmd(t, String.t, CmdRunner.opts) :: :ok | {:error, String.t}
  def cmd(build, cmd, opts \\ []), do:
    CmdRunner.run(cmd, [cd: src_folder(build), logger: CmdRunner.file_logger(log_path(build))] ++ opts)

  @doc "Executes the given command in the build folder, raises on error."
  @spec cmd!(t, String.t, CmdRunner.opts) :: :ok
  def cmd!(build, cmd, opts \\ []), do:
    :ok = cmd(build, cmd, opts)

  @doc "Appends the given output to the build log."
  @spec log(t, iodata) :: :ok
  def log(build, output), do:
    build
    |> log_path()
    |> CmdRunner.file_logger()
    |> apply([["\naircloak_ci: #{output}\n"]])

  @doc "Returns the contents of the build log."
  @spec log_contents(t) :: binary
  def log_contents(build) do
    case File.read(log_path(build)) do
      {:ok, contents} -> contents
      _ -> ""
    end
  end

  @doc "Removes build folders not needed for any pending pull request."
  @spec remove_old_folders([Github.API.pull_request]) :: :ok
  def remove_old_folders(existing_pull_requests) do
    existing_folder_names =
      case File.ls(builds_folder()) do
        {:ok, folders} -> folders
        _ -> []
      end

    expected_folder_names = Enum.map(existing_pull_requests, &to_string(&1.number))

    existing_folder_names
    |> Enum.filter(&(not &1 in expected_folder_names))
    |> Enum.each(&(builds_folder() |> Path.join(&1) |> File.rm_rf()))
  end


  # -------------------------------------------------------------------
  # Build folders
  # -------------------------------------------------------------------

  defp init_folder(build) do
    File.mkdir_p!(src_folder(build))
    File.mkdir_p!(log_folder(build))
    truncate_logs(build)
    build
  end

  defp src_folder(build), do:
    Path.join(build.folder, "src")

  defp log_folder(build), do:
    Path.join(build.folder, "log")

  defp truncate_logs(build), do:
    build.folder |> Path.join("*") |> Path.wildcard() |> Enum.each(&File.write(&1, ""))

  defp builds_folder(), do:
    Application.app_dir(:aircloak_ci, Path.join("priv", "builds"))


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp clone_repo(build) do
    if File.exists?(Path.join(src_folder(build), ".git")) do
      :ok
    else
      CmdRunner.run(
        ~s(git clone git@github.com:#{build.repo.owner}/#{build.repo.name} #{src_folder(build)}),
        timeout: :timer.minutes(1),
        logger: CmdRunner.file_logger(log_path(build))
      )
    end
  end

  defp log_path(build), do:
    build
    |> log_folder()
    |> Path.join("build.log")
end
