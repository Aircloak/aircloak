defmodule Mix.Tasks.AircloakCi.ForceBranchBuild do
  @shortdoc "Force starts the build of the given branch."
  @moduledoc """
  Force starts the build of the given branch.

  This is a convenience task for local debugging and experimenting with CI builds.

  In order to run the task, you need to generate the [Personal access token](https://github.com/settings/tokens).
  Make sure to check all the boxes in the `repo` section.

  Once you have the token, you can start the build for the branch with the following command:

  ```
  mix aircloak_ci.force_branch_build branch_name
  ```

  Note that this command will only work on open branchs.
  """

  use Mix.Task

  # Mix.Task behaviour is not in PLT since Mix is not a runtime dep, so we disable the warning
  @dialyzer :no_undefined_callbacks

  @impl Mix.Task
  def run([name]) do
    Mix.Task.run("app.start")

    case AircloakCI.force_branch_build(name) do
      :ok -> :timer.sleep(:infinity)
      {:error, reason} ->
        Mix.raise("error starting the build: #{reason}")
    end
  end
  def run(_other) do
    Mix.raise("Usage: `mix aircloak_ci.force_branch_build branch_name`")
  end
end
