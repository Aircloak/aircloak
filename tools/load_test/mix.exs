defmodule LoadTest.MixProject do
  use Mix.Project

  def project do
    [
      app: :load_test,
      version: "0.1.0",
      elixir: "~> 1.6",
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  def application do
    [
      extra_applications: [:logger]
    ]
  end

  defp deps do
    [
      {:postgrex, github: "sasa1977/postgrex", tag: "0.13.5", override: true}
    ]
  end
end
