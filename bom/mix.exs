defmodule Bom.Mixfile do
  use Mix.Project

  def project do
    [
      app: :bom,
      version: "0.1.0",
      elixir: "~> 1.3",
      build_embedded: Mix.env() == :prod,
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      aliases: aliases(Mix.env())
    ]
  end

  # Configuration for the OTP application
  #
  # Type "mix help compile.app" for more information
  def application do
    [
      extra_applications: [:ssl, :inets, :logger] ++ dialyzer_deps()
    ]
  end

  defp dialyzer_deps(), do: [:jason]

  # Dependencies can be Hex packages:
  #
  #   {:mydep, "~> 0.3.0"}
  #
  # Or git/path repositories:
  #
  #   {:mydep, git: "https://github.com/elixir-lang/mydep.git", tag: "0.1.0"}
  #
  # Type "mix help deps" for more examples and options
  defp deps do
    [
      {:aircloak_common, path: "../common/elixir"},
      {:tomlex, "0.0.5"}
    ]
  end

  #
  # Aliases are shortcut or tasks specific to the current project.
  # For example, to create, migrate and run the seeds file at once:
  #
  #     $ mix ecto.setup
  #
  # See the documentation for `Mix` for more info on aliases.
  defp aliases(env) when env in [:dev, :test] do
    [
      lint: ["credo --strict --ignore #{Enum.join(ignored_credo_checks(Mix.env()), ",")}"]
    ]
  end

  defp aliases(:prod), do: []

  defp ignored_credo_checks(:test), do: ["ModuleDoc" | ignored_credo_checks(:dev)]

  defp ignored_credo_checks(_), do: ["NameRedeclarationBy", "AliasUsage", "PipeChain", "ABCSize", "Nesting"]
end
