defmodule Central.Mixfile do
  use Mix.Project

  def project do
    [
      app: :central,
      version: "0.0.1",
      elixir: "~> 1.3",
      elixirc_paths: elixirc_paths(Mix.env()),
      compilers: [
        :phoenix,
        :gettext,
        :yecc,
        :leex,
        :erlang,
        :elixir,
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
        site_release: :prod,
        "test.standard": :test,
        dialyze_retry: :dev
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
      mod: {Central, []},
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
      {:ecto, "~> 3.1.7"},
      {:ecto_sql, "~> 3.1.6"},
      {:phoenix, "~> 1.5.0"},
      {:phoenix_pubsub, "~> 2.0"},
      {:postgrex, "~> 0.15.0"},
      {:phoenix_ecto, "~> 4.1"},
      {:phoenix_html, "~> 2.14.0"},
      {:phoenix_live_reload, "~> 1.2", only: :dev},
      {:gettext, "~> 0.9"},
      {:plug_cowboy, "~> 2.1"},
      {:comeonin, "~> 2.5"},
      {:guardian, "~> 1.2.1"},
      {:aircloak_common, path: "../common/elixir"},
      {:inflex, "~> 1.5.0"},
      {:httpoison, "~> 0.12.0"},
      {:quantum, "~> 2.2.7"},
      {:poolboy, ">= 0.0.0"},
      {:jason, "~> 1.0"},
      {:phoenix_gen_socket_client, "~> 2.0", only: :test},
      {:websocket_client, "~> 1.2.4", only: :test}
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
      "test.standard": ["test", "eunit"],
      lint: ["credo --strict --ignore #{Enum.join(ignored_credo_checks(Mix.env()), ",")}"]
    ]
  end

  defp aliases(:prod), do: []

  defp ignored_credo_checks(:test), do: ["ModuleDoc" | ignored_credo_checks(:dev)]

  defp ignored_credo_checks(_), do: ["NameRedeclarationBy", "AliasUsage", "PipeChain", "ABCSize", "Nesting"]

  defp extra_applications(:test), do: common_extra_applications()
  defp extra_applications(:dev), do: common_extra_applications() ++ dialyzer_required_deps()
  defp extra_applications(:prod), do: common_extra_applications()

  defp common_extra_applications(), do: [:logger, :inets, :crontab, :timex]

  # These are indirect dependencies (deps of deps) which are not automatically included in the generated PLT.
  # By adding them explicitly to the applications list, we make sure that they are included in the PLT.
  # This is usually not needed, but in some cases it's required if our code directly relies on
  # types and behaviours from indirect dependencies. In such case, simply add the needed application to
  # this list.
  defp dialyzer_required_deps, do: [:plug, :poolboy]

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
