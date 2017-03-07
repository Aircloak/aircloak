defmodule UserGuides.Mixfile do
  use Mix.Project

  def project do
    [
      app: :user_guides,
      version: "0.1.0",
      elixir: "~> 1.4",
      build_embedded: Mix.env == :prod,
      start_permanent: Mix.env == :prod,
      deps: deps(),
      compilers: [:elixir, :guides, :app],
    ]
  end

  def application do
    [extra_applications: [:logger]]
  end

  defp deps do
    [
      {:earmark, "~> 1.1.1"}
    ]
  end
end
