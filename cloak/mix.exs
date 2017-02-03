defmodule Cloak.Mixfile do
  use Mix.Project

  with version <- File.read!("../VERSION") |> String.trim() do
    def project do
      [
        app: :cloak,
        version: unquote(version),
        elixir: "~> 1.3",
        build_embedded: Mix.env == :prod,
        start_permanent: Mix.env == :prod,
        deps: deps(),
        elixirc_paths: elixirc_paths(Mix.env),
        preferred_cli_env: [
          :test, dialyze: :dev, "coveralls.html": :test, release: :prod,
          dialyze_retry: :dev
        ],
        aliases: aliases(Mix.env),
        test_coverage: [tool: ExCoveralls]
      ]
    end
  end

  @doc "Returns the version of the cloak application"
  @spec version() :: String.t
  def version(), do: Keyword.get(project(), :version)

  def application do
    [
      extra_applications: extra_applications(Mix.env),
      mod: {Cloak, []}
    ]
  end

  defp deps do
    [
      {:aircloak_common, path: "../common/elixir"},
      {:meck, github: "eproxus/meck", tag: "0.8.2", override: true, runtime: false},
      {:postgrex, "~> 0.13"},
      {:mariaex, "~> 0.8"},
      {:phoenix_gen_socket_client, github: "aircloak/phoenix_gen_socket_client"},
      {:websocket_client, github: "sanmiguel/websocket_client", tag: "1.1.0"},
      {:combine, "~> 0.9.2"},
      {:timex, "~> 3.1.3"},
      {:poison, "~> 2.2.0", override: true},
      {:mongodb, "~> 0.2.0"},
      {:lens, "~> 0.2.0"},

      # Test deps

      {:phoenix, "~> 1.1.6", only: :test},
      {:cowboy, "~> 1.0", only: :test},
      {:bypass, "~> 0.5.1", only: :test},
      {:excheck, "~> 0.5.3", only: :test},
      {:triq, github: "triqng/triq", only: :test}
    ]
  end

  defp extra_applications(:test), do: common_extra_applications()
  defp extra_applications(:dev), do: [:os_mon | common_extra_applications()]
  defp extra_applications(:prod), do: [:os_mon | common_extra_applications()]

  defp common_extra_applications(), do:
    [:logger, :runtime_tools, :odbc, :crypto, :ssl, :public_key]

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
    ["NameRedeclarationBy", "AliasUsage", "PipeChain", "ABCSize", "Nesting", "FunctionArity"]
end
