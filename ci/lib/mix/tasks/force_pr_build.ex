defmodule Mix.Tasks.AircloakCi.ForcePrBuild do
  @shortdoc "Force starts the build of the given pull request."
  @moduledoc """
  Force starts the build of the given pull request.

  This is a convenience task for local debugging and experimenting with CI builds.

  In order to run the task, you need to generate the [Personal access token](https://github.com/settings/tokens).
  Make sure to check all the boxes in the `repo` section.

  Once you have the token, you can start the build for the PR with the following command:

  ```
  mix aircloak_ci.force_pr_build pr_number
  ```

  Note that this command will only work on open pull requests.
  """

  use Mix.Task

  # Mix.Task behaviour is not in PLT since Mix is not a runtime dep, so we disable the warning
  @dialyzer :no_undefined_callbacks

  @impl Mix.Task
  def run([number]) do
    Mix.Task.run("app.start")

    case AircloakCI.force_pr_build(String.to_integer(number)) do
      :ok -> :timer.sleep(:infinity)
      {:error, reason} ->
        Mix.raise("error starting the build: #{reason}")
    end
  end
  def run(_other) do
    Mix.raise("Usage: `mix aircloak_ci.force_pr_build pr_number`")
  end
end
