defmodule Cloak.Mixfile do
  use Mix.Project

  def project do
    [
      app: :cloak,
      version: "0.1.0",
      elixir: "~> 1.3",
      build_embedded: Mix.env == :prod,
      start_permanent: Mix.env == :prod,
      deps: deps,
      elixirc_paths: elixirc_paths(Mix.env),
      preferred_cli_env: [
        :test, dialyze: :dev, "coveralls.html": :test,
        dialyze_retry: :dev, check_dependent_apps: :prod
      ],
      aliases: aliases(Mix.env),
      test_coverage: [tool: ExCoveralls]
    ]
  end

  def application do
    [
      applications: applications(Mix.env),
      mod: {Cloak, []}
    ]
  end

  defp deps do
    [
      {:aircloak_common, path: "../common/elixir"},
      {:gproc, "~> 0.5.0"},
      {:meck, github: "eproxus/meck", tag: "0.8.2", override: true, warn_missing: false},
      {:postgrex, "~> 0.11"},
      {:mariaex, "~> 0.7.7"},
      {:phoenix_gen_socket_client, github: "aircloak/phoenix_gen_socket_client"},
      {:websocket_client, github: "sanmiguel/websocket_client", tag: "1.1.0"},
      {:combine, "~> 0.9.2"},
      {:timex, "~> 3.1.3"},
      {:poison, "~> 2.2.0", override: true},
      {:httpoison, "~> 0.9.0"},
      {:poolboy, "~> 1.5", override: true},
      {:mongodb, github: "comtihon/mongodb-erlang"},
      {:lens, "0.0.1"},

      # Test deps

      {:phoenix, "~> 1.1.6", only: :test},
      {:cowboy, "~> 1.0", only: :test},
      {:bypass, "~> 0.5.1", only: :test},
      {:excheck, "~> 0.5", only: :test},
      {:triq, github: "triqng/triq", only: :test}
    ]
  end

  defp applications(:test), do: [:phoenix, :cowboy, :bypass | common_applications()]
  defp applications(:dev), do: [:os_mon | common_applications()]
  defp applications(:prod), do: [:os_mon | common_applications()]

  defp common_applications do
    [
      :logger, :gproc, :aircloak_common, :postgrex, :mariaex,
      :phoenix_gen_socket_client, :websocket_client, :combine,
      :runtime_tools, :httpoison, :timex, :poison, :odbc, :lens,
      :bson, :crypto, :mongodb
    ]
  end

  defp elixirc_paths(:test), do: ["lib", "test/support"]
  defp elixirc_paths(_), do: ["lib"]

  # Aliases are shortcut or tasks specific to the current project.
  # For example, to create, migrate and run the seeds file at once:
  #
  #     $ mix ecto.setup
  #
  # See the documentation for `Mix` for more info on aliases.
  defp aliases(env) when env in [:dev, :test] do
    [
      "lint": ["credo --strict --ignore #{Enum.join(ignored_credo_checks(Mix.env), ",")}"]
    ]
  end
  defp aliases(:prod), do: []

  defp ignored_credo_checks(:test), do:
    ["ModuleDoc" | ignored_credo_checks(:dev)]
  defp ignored_credo_checks(_), do:
    ["NameRedeclarationBy", "AliasUsage", "PipeChain", "ABCSize", "Nesting"]
end
