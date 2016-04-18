defmodule Cloak.Mixfile do
  use Mix.Project

  def project do
    [
      app: :cloak,
      version: "0.0.1",
      elixir: "~> 1.2",
      build_embedded: Mix.env == :prod,
      start_permanent: Mix.env == :prod,
      deps: deps,
      compilers: [:"protobuf.erlang", :yecc, :leex, :erlang, :elixir, :sandbox, :app],
      erlc_options: erlc_options(Mix.env),
      erlc_paths: erlc_paths(Mix.env),
      preferred_cli_env: [
        eunit: :test, proper: :test, "test.standard": :test, dialyze: :dev, "coveralls.html": :test,
        dialyze_retry: :dev
      ],
      eunit_options: [
        :no_tty,
        {:report, {:eunit_progress, [:colored]}}
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
      {:exrm, "~> 1.0"},
      {:meck, github: "eproxus/meck", tag: "0.8.2", override: true},
      {:postgrex, "~> 0.11"},
      {:poolboy, "~> 1.5"},
      {:phoenix_gen_socket_client, github: "aircloak/phoenix_gen_socket_client"},
      {:websocket_client, github: "sanmiguel/websocket_client", tag: "1.1.0"}
    ]
  end

  defp applications(:test), do: common_applications()
  defp applications(:dev), do: [:os_mon | common_applications()]
  defp applications(:prod), do: [:os_mon | common_applications()]

  defp common_applications do
    [
      :logger, :gproc, :aircloak_common, :postgrex, :poolboy,
      :phoenix_gen_socket_client, :websocket_client
    ]
  end

  defp erlc_options(:test), do: [:debug_info, {:d, :TEST}]
  defp erlc_options(:dev), do: [:debug_info]
  defp erlc_options(:prod), do: []

  defp erlc_paths(:test), do: ["test/erlang/*", "src"]
  defp erlc_paths(:dev), do: ["src"]
  defp erlc_paths(:prod), do: ["src"]

  # Aliases are shortcut or tasks specific to the current project.
  # For example, to create, migrate and run the seeds file at once:
  #
  #     $ mix ecto.setup
  #
  # See the documentation for `Mix` for more info on aliases.
  defp aliases(env) when env in [:dev, :test] do
    [
      "test.standard": ["test", "eunit", "proper --level simple"],
      "lint": ["credo --strict"]
    ]
  end
  defp aliases(:prod), do: []
end
