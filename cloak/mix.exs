defmodule Cloak.Mixfile do
  use Mix.Project

  def project do
    [
      app: :cloak,
      version: "0.1.0",
      elixir: "~> 1.2",
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
      {:exrm, "~> 1.0", warn_missing: false},
      {:meck, github: "eproxus/meck", tag: "0.8.2", override: true, warn_missing: false},
      {:postgrex, "~> 0.11"},
      {:poolboy, "~> 1.5"},
      {:phoenix_gen_socket_client, github: "aircloak/phoenix_gen_socket_client"},
      {:websocket_client, github: "sanmiguel/websocket_client", tag: "1.1.0"},
      {:combine, github: "bitwalker/combine", override: true},
      {:timex, "~> 2.1.6", github: "bitwalker/timex"},
      {:poison, "~> 1.5.2"},
      {:httpoison, "~> 0.8.3"},

      # Test deps

      {:phoenix, "~> 1.1.4", only: :test},
      {:cowboy, "~> 1.0", only: :test},
      {:bypass, "~> 0.5.1", only: :test}
    ]
  end

  defp applications(:test), do: [:phoenix, :cowboy, :bypass | common_applications()]
  defp applications(:dev), do: [:os_mon | common_applications()]
  defp applications(:prod), do: [:os_mon | common_applications()]

  defp common_applications do
    [
      :logger, :gproc, :aircloak_common, :postgrex, :poolboy,
      :phoenix_gen_socket_client, :websocket_client, :combine,
      :runtime_tools, :httpoison, :timex, :poison
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
      "lint": ["credo --strict"]
    ]
  end
  defp aliases(:prod), do: []
end
