defmodule Aircloak.File do
  @moduledoc "Utility for reading configuration files."

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "The full path to the config directory"
  defmacro config_dir_path(), do: quote(do: unquote(__MODULE__).config_dir_path(unquote(Mix.Project.config()[:app])))

  @doc "The full path to the config directory for a given application"
  @spec config_dir_path(atom) :: String.t()
  def config_dir_path(app), do: Path.join([Application.app_dir(app, "priv"), "config"])

  @doc "Reads a file from the configuration folder for the calling application"
  defmacro read_config_file(path_segment),
    do:
      quote(
        do:
          unquote(__MODULE__).read_config_file(
            unquote(Mix.Project.config()[:app]),
            unquote(path_segment)
          )
      )

  @doc "Reads a file from the configuration folder for a given application"
  @spec read_config_file(atom, String.t()) :: {:ok, Map.t()} | {:error, String.t()}
  def read_config_file(app, path_segment),
    do:
      config_path(app, path_segment)
      |> File.read!()
      |> Aircloak.Json.safe_decode()

  @doc "Lists all files inside a config directory for the calling application"
  defmacro ls(path_segment),
    do: quote(do: unquote(__MODULE__).ls(unquote(Mix.Project.config()[:app]), unquote(path_segment)))

  @doc "Lists all files inside a config directory for a given application"
  @spec ls(atom, String.t()) :: {:ok, [String.t()]} | {:error, :file.posix()}
  def ls(app, path_segment), do: File.ls(config_path(app, path_segment))

  @doc "Produces a human warning from a posix error code"
  @spec humanize_posix_error(:file.posix()) :: String.t()
  def humanize_posix_error(:eacces), do: "permission denied"
  def humanize_posix_error(:ebusy), do: "device or resource busy"
  def humanize_posix_error(:edquot), do: "quota exceeded"
  def humanize_posix_error(:eexist), do: "file exists"
  def humanize_posix_error(:emfile), do: "too many open files"
  def humanize_posix_error(:emlink), do: "too many links"
  def humanize_posix_error(:enodev), do: "no such device"
  def humanize_posix_error(:enoent), do: "no such file or directory"
  def humanize_posix_error(:enomem), do: "out of memory"
  def humanize_posix_error(:enospc), do: "no space left on device"
  def humanize_posix_error(:enxio), do: "no such device or address"
  def humanize_posix_error(:eperm), do: "operation not permitted"
  def humanize_posix_error(:exdev), do: "cross-device link"
  def humanize_posix_error(other), do: "unclassified reason: `#{other}`"

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp config_path(app, path_segment), do: Path.join([config_dir_path(app), path_segment])
end
