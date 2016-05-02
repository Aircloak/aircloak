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
      aliases: [
        "test.standard": ["test", "eunit"],
        "lint": ["credo --strict"]
      ],
      preferred_cli_env: [
        "coveralls.html": :test, dialyze: :dev, docs: :dev, "test.standard": :test, dialyze_retry: :dev,
        check_dependent_apps: :prod
      ],
      test_coverage: [tool: ExCoveralls]
    ]
  end

  def application do
    [applications: [:logger, :poolboy, :aircloak_common]]
  end

  defp deps do
    [
      {:poolboy, "~> 1.5.0"},
      {:aircloak_common, path: "../elixir"}
    ]
  end
end
