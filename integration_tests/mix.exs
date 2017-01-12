defmodule IntegrationTests.Mixfile do
  use Mix.Project

  def project do
    [
      app: :integration_tests,
      version: "0.1.0",
      elixir: "~> 1.3",
      build_embedded: Mix.env == :prod,
      start_permanent: Mix.env == :prod,
      deps: deps(),
      elixirc_paths: elixirc_paths(Mix.env),
    ]
  end

  def application do
    [extra_applications: [:logger, :odbc]]
  end

  defp deps do
    [
      {:cloak, path: "../cloak"},
      {:air, path: "../air"},
    ]
  end

  defp elixirc_paths(:test), do: ["lib", "test/support"]
  defp elixirc_paths(_), do: ["lib"]
end
