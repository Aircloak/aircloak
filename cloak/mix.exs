defmodule Cloak.Mixfile do
  use Mix.Project

  def project do
    [
      app: :cloak,
      version: File.read!("../VERSION") |> String.trim(),
      elixir: "~> 1.3",
      build_embedded: Mix.env == :prod,
      start_permanent: Mix.env == :prod,
      deps: deps(),
      deps_path: Path.join(["deps", otp_version(), elixir_version()]),
      elixirc_paths: elixirc_paths(Mix.env),
      preferred_cli_env: [
        dialyze: :dev, "coveralls.html": :test, release: :prod, dialyze_retry: :dev, compliance: :test
      ],
      aliases: aliases(Mix.env),
      test_coverage: [tool: ExCoveralls]
    ]
  end

  def application do
    [
      extra_applications: extra_applications(Mix.env),
      mod: {Cloak, []}
    ]
  end

  defp deps do
    [
      {:aircloak_common, path: "../common/elixir"},
      {:quantum, "~> 1.9"},
      {:meck, github: "eproxus/meck", tag: "0.8.2", override: true, runtime: false},
      {:postgrex, "~> 0.13"},
      {:mariaex, "~> 0.8"},
      {:phoenix_gen_socket_client, "~> 2.0"},
      {:websocket_client, "~> 1.2.4"},
      {:combine, "~> 0.9.6"},
      {:timex, "~> 3.1"},
      {:poison, github: "sasa1977/poison", override: true},
      {:mongodb, "~> 0.3.0"},
      {:lens, "~> 0.4.0"},
      {:backoff, "~> 1.1.3"},
      {:jiffy, "~> 0.14.1"},
      {:tds, github: "cristianberneanu/tds"},
      {:file_system, "~> 0.2.2"},

      # Hackney is not a direct dependency of ours, but we need it to be at version 1.8.6 or more recent
      # in order to build under Erlang 20.0. Earlier versions indirectly included too old versions of a
      # unicode compatibility layer that doesn't jell with our version of Erlang.
      {:hackney, ">= 1.8.6", override: true},

      # Test deps

      {:phoenix, "~> 1.3.0", only: :test},
      {:cowboy, "~> 1.0", only: :test},
      {:bypass, "~> 0.5.1", only: :test},
      {:excheck, "~> 0.5.3", only: :test},
      {:triq, github: "triqng/triq", only: :test},

      # Only used for perf tests
      {:httpoison, "~> 0.13.0", runtime: false},
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
  defp aliases(_), do: []

  defp ignored_credo_checks(:test), do:
    ["ModuleDoc", "DuplicatedCode" | ignored_credo_checks(:dev)]
  defp ignored_credo_checks(_), do:
    ["NameRedeclarationBy", "AliasUsage", "PipeChain", "ABCSize", "Nesting", "FunctionArity"]

  defp otp_version(), do:
    [:code.root_dir(), "releases", :erlang.system_info(:otp_release), "OTP_VERSION"]
    |> Path.join()
    |> File.read!()
    |> String.trim("\n")

  defp elixir_version(), do:
    System.version()
end
