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
      {:protobuffs, github: "basho/erlang_protobuffs", tag: "0.8.2"},
      {:ex_doc, "~> 0.11"},
      {:earmark, "~> 0.2"},
      {:meck, github: "eproxus/meck", tag: "0.8.2", override: true},
      {:credo, "~> 0.3.0"},
      {:eunit_formatters, "~> 0.3.0"},
      {:dialyze, "~> 0.2.1"},
      {:excoveralls, "~> 0.5"},
      {:proper, github: "manopapad/proper"},
      {:phoenix, "~> 1.1.4", only: :test},
      {:cowboy, "~> 1.0", only: :test}
    ]
  end

  defp elixirc_paths(:test), do: ["lib", "test/support"]
  defp elixirc_paths(_), do: ["lib"]

  defp applications(:test), do: [:logger, :gproc, :phoenix, :cowboy, :poison]
  defp applications(_), do: [:logger, :gproc, :poison]
end
