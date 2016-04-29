defmodule JsSandbox.Mixfile do
  use Mix.Project

  def project do
    [
      app: :js_sandbox,
      version: "0.0.1",
      elixir: "~> 1.2",
      build_embedded: Mix.env == :prod,
      start_permanent: Mix.env == :prod,
      deps: deps,
      compilers: [:erlang, :elixir, :c_src, :app],
    ]
  end

  def application do
    [applications: [:logger]]
  end

  defp deps do
    []
  end
end
