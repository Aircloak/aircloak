defmodule Mix.Tasks.AircloakCi.ForceBuild do
  @shortdoc "Force starts the desired build."
  @moduledoc """
  Force starts the desired build.

  This is a convenience task for local debugging and experimenting with CI builds.

  In order to run the task, you need to generate the [Personal access token](https://github.com/settings/tokens).
  Make sure to check all the boxes in the `repo` section.

  Once you have the token, you can use the following commands

  ```
  mix aircloak_ci.force_build branch branch_name job_name
  mix aircloak_ci.force_build pr pr_number job_name
  ```

  Where `job_name` is one of `cloak_test`, `compliance`.

  In addition, you can force a task on a local project with:

  ```
  mix aircloak_ci.force_build local job_name
  ```

  This is useful when you're testing local changes.
  """

  use Mix.Task

  # Mix.Task behaviour is not in PLT since Mix is not a runtime dep, so we disable the warning
  @dialyzer :no_undefined_callbacks

  # -------------------------------------------------------------------
  # Mix.Task callbacks
  # -------------------------------------------------------------------

  @impl Mix.Task
  def run(["local", job_name]), do: run(["local", repo_root_path(), job_name])

  def run([target_type, target_id, job_name]) do
    Mix.Task.run("app.start")

    case AircloakCI.force_build(target_type, target_id, job_name) do
      :ok ->
        :timer.sleep(:infinity)

      {:error, reason} ->
        Mix.raise("error starting the build: #{reason}")
    end
  end

  def run(_other) do
    Mix.raise("Usage: `mix aircloak_ci.force_build target_type target_id job_name`")
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp repo_root_path(), do: :os.cmd('git rev-parse --show-toplevel') |> to_string() |> String.trim()
end
