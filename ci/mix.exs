defmodule AircloakCI.Mixfile do
  use Mix.Project

  def project do
    [
      app: :aircloak_ci,
      version: "0.1.0",
      elixir: "~> 1.5",
      start_permanent: Mix.env == :prod,
      deps: deps(),
      aliases: aliases(Mix.env),
      preferred_cli_env: [dialyze: :dev, release: :prod],
      elixirc_paths: elixirc_paths(Mix.env),
    ]
  end

  def application do
    [
      extra_applications: [:logger, :runtime_tools],
      mod: {AircloakCI.Application, []}
    ]
  end

  defp deps do
    [
      {:httpoison, "~> 0.13.0"},
      {:poison, "~> 2.0"},
      {:aircloak_common, path: "../common/elixir"},
      {:erlexec, "~> 1.7", only: [:dev, :prod]},
      {:jobs, "~> 0.7"},
      {:recon, "~> 2.0"},
    ]
  end

  defp aliases(env) when env in [:dev, :test] do
    [
      "lint": ["credo --strict --ignore #{Enum.join(ignored_credo_checks(Mix.env), ",")}"]
    ]
  end
  defp aliases(_), do: []

  defp ignored_credo_checks(:test), do:
    ["ModuleDoc", "DuplicatedCode" | ignored_credo_checks(:dev)]
  defp ignored_credo_checks(_), do:
    ["NameRedeclarationBy", "AliasUsage", "PipeChain", "ABCSize", "Nesting", "FunctionArity"]

  defp elixirc_paths(:test), do: ["lib", "test/support"]
  defp elixirc_paths(_), do: ["lib"]
end
