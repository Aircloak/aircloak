defmodule IntegrationTests.Mixfile do
  use Mix.Project

  def project do
    [app: :integration_tests,
     version: "0.1.0",
     elixir: "~> 1.3",
     build_embedded: Mix.env == :prod,
     start_permanent: Mix.env == :prod,
     deps: deps()]
  end

  def application do
    [applications: [:logger, :cloak]]
  end

  defp deps do
    [
      {:cloak, path: "../cloak"},
    ]
  end
end
