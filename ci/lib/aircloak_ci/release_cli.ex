defmodule AircloakCI.ReleaseCLI do
  @moduledoc "Facade module for interacting with the OTP release through the release bash script."


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Force starts the build of the given pull request."
  @spec force_start_build(pos_integer) :: :ok
  def force_start_build(pull_request_number) do
    case AircloakCI.force_build(pull_request_number) do
      :ok -> IO.puts("build started successfully")
      {:error, reason} -> IO.puts("error: #{reason}")
    end
  end

  @doc "Prints the build log of the given pull request."
  @spec print_build_log(pos_integer) :: :ok
  def print_build_log(pull_request_number) do
    AircloakCI.Github.pull_request("aircloak", "aircloak", pull_request_number)
    |> AircloakCI.LocalProject.for_pull_request()
    |> AircloakCI.LocalProject.log_contents()
    |> IO.puts()
  end

  @doc "Puts the system in the soft termination mode."
  @spec start_soft_termination() :: :ok
  def start_soft_termination() do
    AircloakCI.start_soft_termination()
    IO.puts "the system is marked for soft termination"
  end
end
