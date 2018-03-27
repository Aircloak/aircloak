defmodule Aircloak.ElixirCommon.Mixfile do
  use Mix.Project

  def project do
    [
      app: :aircloak_common,
      version: "0.0.1",
      elixir: "~> 1.3",
      elixirc_paths: elixirc_paths(Mix.env()),
      erlc_paths: erlc_paths(Mix.env()),
      erlc_options: erlc_options(Mix.env()),
      build_embedded: Mix.env() == :prod,
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      aliases: [
        "test.standard": ["test", "eunit", "proper --level simple"],
        lint: ["credo --strict --ignore #{Enum.join(ignored_credo_checks(Mix.env()), ",")}"]
      ],
      preferred_cli_env: [
        eunit: :test,
        proper: :test,
        "test.standard": :test,
        dialyze: :dev,
        "coveralls.html": :test,
        dialyze_retry: :dev
      ],
      test_coverage: [tool: ExCoveralls],
      eunit_options: [
        :no_tty,
        {:report, {:eunit_progress, [:colored]}}
      ]
    ]
  end

  def application do
    [
      mod: {Aircloak, []},
      extra_applications: [:logger, :poison | dialyzer_deps()]
    ]
  end

  defp deps do
    [
      {:poison, "~> 2.2.0"},
      {:lens, "~> 0.6.0"},
      {:ex_doc, "~> 0.16", runtime: false},
      {:meck, github: "eproxus/meck", tag: "0.8.2", override: true, runtime: false},
      {:credo, "~> 0.8.10", runtime: false},
      {:eunit_formatters, "~> 0.3.0", runtime: false},
      {:proper, "~> 1.2", runtime: false},
      {:dialyze, "~> 0.2.1", runtime: false},
      {:excoveralls, "~> 0.5.5", runtime: false},
      {:distillery, "~> 1.1.0", runtime: false},
      {:phoenix, "~> 1.1.6", only: :test},
      {:cowboy, "~> 1.0", only: :test},
      {:phoenix_gen_socket_client, "~> 2.0", optional: true},
      {:ex_crypto, "~> 0.9.0"},
      {:timex, ">= 3.1.10 and < 4.0.0"},
      {:ex_json_schema, "~> 0.5.6"}
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

  defp ignored_credo_checks(:test), do: ["ModuleDoc" | ignored_credo_checks(:dev)]

  defp ignored_credo_checks(_),
    do: ["NameRedeclarationBy", "AliasUsage", "PipeChain", "ABCSize", "Nesting"]

  if Mix.env() == :dev do
    defp dialyzer_deps(), do: [:phoenix_gen_socket_client]
  else
    defp dialyzer_deps(), do: []
  end
end
