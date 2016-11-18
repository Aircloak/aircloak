defmodule Aircloak.ElixirCommon.Mixfile do
  use Mix.Project

  def project do
    [
      app: :aircloak_common,
      version: "0.0.1",
      elixir: "~> 1.3",
      elixirc_paths: elixirc_paths(Mix.env),
      erlc_paths: erlc_paths(Mix.env),
      erlc_options: erlc_options(Mix.env),
      build_embedded: Mix.env == :prod,
      start_permanent: Mix.env == :prod,
      deps: deps,
      aliases: [
        "test.standard": ["test", "eunit", "proper --level simple"],
        "lint": ["credo --strict --ignore #{Enum.join(ignored_credo_checks(Mix.env), ",")}"]
      ],
      preferred_cli_env: [
        eunit: :test, proper: :test, "test.standard": :test, dialyze: :dev,
        "coveralls.html": :test, dialyze_retry: :dev
      ],
      test_coverage: [tool: ExCoveralls],
      eunit_options: [
        :no_tty,
        {:report, {:eunit_progress, [:colored]}}
      ]
    ]
  end

  def application do
    [applications: applications(Mix.env)]
  end

  defp deps do
    [
      {:poison, "~> 2.2.0"},
      {:gproc, "~> 0.5.0"},
      {:protobuffs, github: "basho/erlang_protobuffs", tag: "0.8.2", warn_missing: false},
      {:ex_doc, "~> 0.13", warn_missing: false},
      {:earmark, "~> 1.0", warn_missing: false},
      {:meck, github: "eproxus/meck", tag: "0.8.2", override: true, warn_missing: false},
      {:credo, "~> 0.4.8", warn_missing: false},
      {:eunit_formatters, "~> 0.3.0", warn_missing: false},
      {:proper, github: "manopapad/proper", warn_missing: false},
      {:dialyze, "~> 0.2.1", warn_missing: false},
      {:excoveralls, "~> 0.5.5", warn_missing: false},
      {:exrm, "~> 1.0.8", warn_missing: false},
      {:ranch, "~> 1.0", optional: true, only: [:dev, :test]},
      {:phoenix, "~> 1.1.6", only: :test},
      {:cowboy, "~> 1.0", only: :test}
    ]
  end

  defp elixirc_paths(:test), do: ["lib", "test/support"]
  defp elixirc_paths(_), do: ["lib"]

  defp erlc_paths(:test), do: ["test/erlang/", "src"]
  defp erlc_paths(:dev), do: ["src"]
  defp erlc_paths(:prod), do: ["src"]

  defp erlc_options(:test), do: [:debug_info, {:d, :TEST}]
  defp erlc_options(:dev), do: [:debug_info]
  defp erlc_options(:prod), do: []

  defp applications(:test), do: [:logger, :gproc, :phoenix, :cowboy, :poison, :ex_unit, :ranch]
  defp applications(:dev), do: [:ranch | applications(:prod)]
  defp applications(_), do: [:logger, :gproc, :poison]

  defp ignored_credo_checks(:test), do:
    ["ModuleDoc" | ignored_credo_checks(:dev)]
  defp ignored_credo_checks(_), do:
    ["NameRedeclarationBy", "AliasUsage", "PipeChain", "ABCSize", "Nesting"]
end
