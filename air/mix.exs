defmodule Air.Mixfile do
  use Mix.Project

  def project do
    [
      app: :air,
      version: File.read!("../VERSION") |> String.trim(),
      elixir: "~> 1.3",
      elixirc_paths: elixirc_paths(Mix.env()),
      compilers: [
        :phoenix,
        :gettext,
        :yecc,
        :leex,
        :erlang,
        :elixir,
        :auto_completion,
        :app
      ],
      build_embedded: Mix.env() == :prod,
      start_permanent: Mix.env() == :prod,
      aliases: aliases(Mix.env()),
      deps: deps(),
      elixirc_options: elixirc_options(Mix.env()),
      erlc_paths: erlc_paths(Mix.env()),
      erlc_options: erlc_options(Mix.env()),
      eunit_options: [
        :no_tty,
        {:report, {:eunit_progress, [:colored]}}
      ],
      preferred_cli_env: [
        eunit: :test,
        "coveralls.html": :test,
        dialyze: :dev,
        docs: :dev,
        release: :prod,
        "phx.digest": :prod,
        "test.standard": :test,
        dialyze_retry: :dev,
        version: :prod
      ],
      test_coverage: [tool: ExCoveralls],
      docs: [
        extras: ["README.md"]
      ]
    ]
  end

  # Configuration for the OTP application.
  #
  # Type `mix help compile.app` for more information.
  def application do
    [
      mod: {Air, []},
      extra_applications: extra_applications(Mix.env())
    ]
  end

  # Specifies which paths to compile per environment.
  defp elixirc_paths(:test), do: ["lib", "test/support"]
  defp elixirc_paths(_), do: ["lib"]

  # Specifies your project dependencies.
  #
  # Type `mix help deps` for examples and options.
  defp deps do
    [
      {:ecto, "~> 3.5.0"},
      {:ecto_sql, "~> 3.5.0"},
      {:earmark, "~> 1.2", override: true},
      {:postgrex, "~> 0.15.7", override: true},
      {:poolboy, "~> 1.5.2"},
      {:phoenix, "~> 1.5.0", override: true},
      {:phoenix_pubsub, "~> 2.0"},
      {:phoenix_ecto, "~> 4.2"},
      {:phoenix_html, "~> 2.14"},
      {:phoenix_live_reload, "~> 1.2", only: :dev},
      {:phoenix_gen_socket_client, "~> 3.1"},
      {:websocket_client, "~> 1.2.4"},
      {:gettext, "~> 0.9"},
      {:plug_cowboy, "~> 2.1"},
      {:comeonin, "~> 2.5"},
      {:aircloak_common, path: "../common/elixir"},
      {:inflex, "~> 1.5.0"},
      {:csv, "~> 1.4.2"},
      {:phoenix_mtm, "~> 1.0.0"},
      {:scrivener_ecto, "~> 2.2", override: true},
      {:decimal, "~> 1.4"},
      {:remote_ip, "~> 0.1.4"},
      {:ecto_enum, "~> 1.4"},
      {:jiffy, "~> 1.0"},
      {:parent, "~> 0.4.0"},
      {:combine, "~> 0.10.0"},
      {:epgsql, "~> 4.1"},
      {:bom, path: "../bom", runtime: false, only: :dev},
      {:zxcvbn, "~> 0.1.3"},
      {:httpoison, "~> 1.7"},
      {:jason, "~> 1.1"},
      {:phoenix_live_view, "~> 0.15.7"},
      {:phoenix_live_dashboard, "~> 0.4.0"},
      {:telemetry_poller, "~> 0.4"},
      {:telemetry_metrics, "~> 0.4"},
      {:floki, ">= 0.0.0", only: :test}
    ]
  end

  # Aliases are shortcut or tasks specific to the current project.
  # For example, to create, migrate and run the seeds file at once:
  #
  #     $ mix ecto.setup
  #
  # See the documentation for `Mix` for more info on aliases.
  defp aliases(env) when env in [:dev, :test] do
    [
      rollback: ["app.start", "ecto.rollback"],
      migrate: ["app.start", "ecto.migrate"],
      seed: ["app.start", "run priv/repo/seeds.exs"],
      lint: ["credo --strict --ignore #{Enum.join(ignored_credo_checks(), ",")}"]
    ]
  end

  defp aliases(:prod), do: [release: ~w(release_assets phx.digest release)]

  defp extra_applications(:test), do: [:odbc | extra_common_applications()]
  defp extra_applications(:dev), do: extra_common_applications() ++ dialyzer_required_deps()
  defp extra_applications(:prod), do: extra_common_applications()

  defp extra_common_applications(), do: [:logger, :inets, :eldap, :jason, :timex, :os_mon]

  # These are indirect dependencies (deps of deps) which are not automatically included in the generated PLT.
  # By adding them explicitly to the applications list, we make sure that they are included in the PLT.
  # This is usually not needed, but in some cases it's required if our code directly relies on
  # types and behaviours from indirect dependencies. In such case, simply add the needed application to
  # this list.
  defp dialyzer_required_deps, do: [:plug, :poolboy, :ranch]

  defp ignored_credo_checks(), do: ["AliasOrder"]

  defp elixirc_options(:test), do: [debug_info: true, docs: true] ++ common_elixirc_options()
  defp elixirc_options(:dev), do: [debug_info: true, docs: true] ++ common_elixirc_options()
  defp elixirc_options(:prod), do: common_elixirc_options()

  defp common_elixirc_options, do: [ignore_module_conflict: true]

  defp erlc_paths(:test), do: ["test/erlang", "src"]
  defp erlc_paths(:dev), do: ["src"]
  defp erlc_paths(:prod), do: ["src"]

  defp erlc_options(:test), do: [:debug_info, {:d, :TEST}]
  defp erlc_options(:dev), do: [:debug_info]
  defp erlc_options(:prod), do: []
end
