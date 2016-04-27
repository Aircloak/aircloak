defmodule Mix.Tasks.CheckDependentApps do
  use Mix.Task
  @shortdoc "Verifies if all applications are specified in the application list"
  @moduledoc "Verifies if all applications are specified in the application list."

  # Mix.Task behaviour is not in PLT since Mix is not a runtime dep, so we disable the warning
  @dialyzer :no_undefined_callbacks

  @doc false
  def run(_args) do
    case ReleaseManager.Deps.print_missing_applications() do
      "" ->
        IO.puts("Dependency check succeeded!")
      output ->
        [
          "Some dependencies are not provided in the application list: #{output}\n",
          "  - runtime dependencies should be added to the application list.\n",
          "  - dev/test only dependencies should be specified with the `only` option:",
          "      {:phoenix_live_reload, \"~> 1.0\", only: :dev}\n",
          "  - compile-time only dependencies should be specified with the `warn_missing` option:",
          "      {:exrm, \"~> 1.0\", warn_missing: false}\n"
        ]
        |> Enum.join("\n")
        |> Mix.raise()
    end
  end
end
