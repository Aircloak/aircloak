defmodule AircloakCI.ReleaseCLI do
  @moduledoc "Facade module for interacting with the OTP release through the release bash script."


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Force starts the build of the given pull request."
  @spec force_build(String.t, String.t, String.t) :: :ok
  def force_build(target_type, target_id, job_name) do
    case AircloakCI.force_build(target_type, target_id, job_name) do
      :ok -> IO.puts("build started successfully")
      {:error, reason} -> IO.puts("error: #{reason}")
    end
  end

  @doc "Prints the build log of the given pull request."
  @spec print_build_log(String.t, String.t, String.t) :: :ok
  def print_build_log(target_type, target_id, job_name) do
    case File.read(AircloakCI.LocalProject.log_file(target_type, target_id, job_name)) do
      {:ok, contents} -> IO.puts contents
      {:error, _} -> :ok
    end
  end
end
