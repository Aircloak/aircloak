defmodule AircloakCI.ReleaseCLI do
  @moduledoc "Facade module for interacting with the OTP release through the release bash script."


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Force starts the build of the given pull request."
  @spec force_pr_build(pos_integer) :: :ok
  def force_pr_build(pull_request_number) do
    case AircloakCI.force_pr_build(pull_request_number) do
      :ok -> IO.puts("build started successfully")
      {:error, reason} -> IO.puts("error: #{reason}")
    end
  end

  @doc "Force starts the build of the given branch."
  @spec force_branch_build(String.t) :: :ok
  def force_branch_build(branch_name) do
    case AircloakCI.force_branch_build(branch_name) do
      :ok -> IO.puts("build started successfully")
      {:error, reason} -> IO.puts("error: #{reason}")
    end
  end

  @doc "Prints the build log of the given pull request."
  @spec print_build_log(pos_integer) :: :ok
  def print_build_log(pull_request_number) do
    AircloakCI.Github.pull_request("aircloak", "aircloak", pull_request_number)
    |> AircloakCI.LocalProject.for_pull_request()
    |> AircloakCI.LocalProject.log_contents("compliance")
    |> IO.puts()
  end
end
