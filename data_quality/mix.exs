defmodule DataQuality.MixProject do
  use Mix.Project

  def project do
    [
      app: :data_quality,
      version: "0.1.0",
      elixir: "~> 1.6",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      aliases: aliases(Mix.env())
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger, :postgrex, :inets, :ssl]
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:aircloak_common, path: "../common/elixir"},
      {:postgrex, "~> 0.13.5"}
    ]
  end

  defp aliases(env) when env in [:dev, :test] do
    [
      lint: ["credo --strict"]
    ]
  end

  defp aliases(:prod), do: []
end
