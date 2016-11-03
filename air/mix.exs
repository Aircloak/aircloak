defmodule Air.Mixfile do
  use Mix.Project

  def project do
    [
      app: :air,
      version: "0.0.1",
      elixir: "~> 1.3",
      elixirc_paths: elixirc_paths(Mix.env),
      compilers: [
        :phoenix, :gettext, :yecc, :leex, :erlang, :elixir, :user_docs, :app
      ],
      build_embedded: Mix.env == :prod,
      start_permanent: Mix.env == :prod,
      aliases: aliases(Mix.env),
      deps: deps,
      elixirc_options: elixirc_options(Mix.env),
      erlc_paths: erlc_paths(Mix.env),
      erlc_options: erlc_options(Mix.env),
      eunit_options: [
        :no_tty,
        {:report, {:eunit_progress, [:colored]}}
      ],
      preferred_cli_env: [
        eunit: :test, "coveralls.html": :test, dialyze: :dev, docs: :dev, release: :prod,
        "phoenix.digest": :prod, site_release: :prod, "test.standard": :test, dialyze_retry: :dev,
        check_dependent_apps: :prod
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
      applications: applications(Mix.env)
    ]
  end

  # Specifies which paths to compile per environment.
  defp elixirc_paths(:test), do: ["lib", "web", "test/support"]
  defp elixirc_paths(_),     do: ["lib", "web"]

  # Specifies your project dependencies.
  #
  # Type `mix help deps` for examples and options.
  defp deps do
    [
      {:phoenix, "~> 1.2.1"},
      {:phoenix_pubsub, "~> 1.0"},
      {:postgrex, ">= 0.0.0"},
      {:phoenix_ecto, "~> 3.0"},
      {:phoenix_html, "~> 2.4"},
      {:phoenix_live_reload, "~> 1.0", only: :dev},
      {:phoenix_gen_socket_client, github: "aircloak/phoenix_gen_socket_client"},
      {:websocket_client, github: "sanmiguel/websocket_client", tag: "1.1.0"},
      {:gettext, "~> 0.9"},
      {:cowboy, "~> 1.0"},
      {:comeonin, "~> 2.5"},
      {:guardian, "~> 0.13.0"},
      {:lhttpc, github: "esl/lhttpc", override: true},
      {:hackney, "~> 1.5.0"},
      {:exrm, "~> 1.0.8", warn_missing: false},
      {:timex, "~> 2.2"},
      {:aircloak_common, path: "../common/elixir"},
      {:inflex, "~> 1.5.0"},
      {:csv, "~> 1.4.2"},
      {:phoenix_mtm, "~> 0.5.0"}
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
      "rollback": ["app.start", "ecto.rollback"],
      "migrate": ["app.start", "ecto.migrate"],
      "seed": ["app.start", "run priv/repo/seeds.exs"],
      "test.standard": ["test", "eunit"],
      "lint": ["credo --strict --ignore #{Enum.join(ignored_credo_checks(Mix.env), ",")}"]
    ]
  end
  defp aliases(:prod), do: []

  defp ignored_credo_checks(:test), do:
    ["ModuleDoc" | ignored_credo_checks(:dev)]
  defp ignored_credo_checks(_), do:
    ["NameRedeclarationBy", "AliasUsage", "PipeChain", "ABCSize", "Nesting"]

  defp applications(:test), do: [:odbc | common_applications()]
  defp applications(:dev), do: common_applications() ++ dialyzer_required_deps()
  defp applications(:prod), do: common_applications()

  defp common_applications do
    [
      :phoenix, :phoenix_html, :cowboy, :logger, :gettext, :phoenix_ecto, :postgrex, :comeonin,
      :lhttpc, :hackney, :guardian, :inets, :timex, :aircloak_common, :inflex, :csv,
      :phoenix_pubsub, :phoenix_mtm, :ranch, :phoenix_gen_socket_client, :websocket_client,
    ]
  end

  # These are indirect dependencies (deps of deps) which are not automatically included in the generated PLT.
  # By adding them explicitly to the applications list, we make sure that they are included in the PLT.
  # This is usually not needed, but in some cases it's required if our code directly relies on
  # types and behaviours from indirect dependencies. In such case, simply add the needed application to
  # this list.
  defp dialyzer_required_deps, do: [:plug, :poolboy]

  defp elixirc_options(:test), do: [debug_info: true, docs: true] ++ common_elixirc_options
  defp elixirc_options(:dev), do: [debug_info: true, docs: true] ++ common_elixirc_options
  defp elixirc_options(:prod), do: common_elixirc_options

  defp common_elixirc_options, do: [ignore_module_conflict: true]

  defp erlc_paths(:test), do: ["test/erlang", "src"]
  defp erlc_paths(:dev), do: ["src"]
  defp erlc_paths(:prod), do: ["src"]

  defp erlc_options(:test), do: [:debug_info, {:d, :TEST}]
  defp erlc_options(:dev), do: [:debug_info]
  defp erlc_options(:prod), do: []
end
