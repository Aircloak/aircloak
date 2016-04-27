defmodule Aircloak.ElixirCommon.Mixfile do
  use Mix.Project

  def project do
    [
      app: :aircloak_common,
      version: "0.0.1",
      elixir: "~> 1.2",
      elixirc_paths: elixirc_paths(Mix.env),
      build_embedded: Mix.env == :prod,
      start_permanent: Mix.env == :prod,
      deps: deps,
      aliases: [
        "test.standard": ["test", "eunit", "proper --level simple"],
        "lint": ["credo --strict"]
      ],
      preferred_cli_env: [
        eunit: :test, proper: :test, "test.standard": :test, dialyze: :dev,
        "coveralls.html": :test, dialyze_retry: :dev
      ],
      test_coverage: [tool: ExCoveralls]
    ]
  end

  def application do
    [applications: applications(Mix.env)]
  end

  defp deps do
    [
      {:poison, "~> 1.5"},
      {:gproc, "~> 0.5.0"},
      {:protobuffs, github: "basho/erlang_protobuffs", tag: "0.8.2", warn_missing: false},
      {:ex_doc, "~> 0.11", warn_missing: false},
      {:earmark, "~> 0.2", warn_missing: false},
      {:meck, github: "eproxus/meck", tag: "0.8.2", override: true, warn_missing: false},
      {:credo, "~> 0.3.0", warn_missing: false},
      {:eunit_formatters, "~> 0.3.0", warn_missing: false},
      {:proper, github: "manopapad/proper", warn_missing: false},
      {:dialyze, "~> 0.2.1", warn_missing: false},
      {:excoveralls, "~> 0.5", warn_missing: false},
      {:exrm, "~> 1.0", warn_missing: false},
      {:phoenix, "~> 1.1.4", only: :test},
      {:cowboy, "~> 1.0", only: :test}
    ]
  end

  defp elixirc_paths(:test), do: ["lib", "test/support"]
  defp elixirc_paths(_), do: ["lib"]

  defp applications(:test), do: [:logger, :gproc, :phoenix, :cowboy, :poison]
  defp applications(_), do: [:logger, :gproc, :poison]
end
