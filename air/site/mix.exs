defmodule Air.Mixfile do
  use Mix.Project

  def project do
    [
      app: :air,
      version: "0.0.1",
      elixir: "~> 1.0",
      elixirc_paths: elixirc_paths(Mix.env),
      compilers: [:phoenix, :gettext] ++ Mix.compilers,
      build_embedded: Mix.env == :prod,
      start_permanent: Mix.env == :prod,
      aliases: aliases,
      deps: deps,
      erlc_paths: erlc_paths(Mix.env),
      erlc_options: erlc_options(Mix.env),
      eunit_options: [
        :no_tty,
        {:report, {:eunit_progress, [:colored]}}
      ],
      preferred_cli_env: [
        eunit: :test, "coveralls.html": :test, dialyze: :dev, docs: :dev, release: :prod,
        "phoenix.digest": :prod
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
      applications: [
        :phoenix, :phoenix_html, :cowboy, :logger, :gettext, :phoenix_ecto, :postgrex, :comeonin,
        :lhttpc, :etcd, :hackney, :guardian
      ]
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
      {:phoenix, "~> 1.1.4"},
      {:postgrex, ">= 0.0.0"},
      {:phoenix_ecto, "~> 2.0"},
      {:phoenix_html, "~> 2.4"},
      {:phoenix_live_reload, "~> 1.0", only: :dev},
      {:gettext, "~> 0.9"},
      {:cowboy, "~> 1.0"},
      {:comeonin, "~> 2.0"},
      {:guardian, "~> 0.10.0"},
      {:lhttpc, github: "esl/lhttpc", override: true},
      {:etcd, github: "spilgames/etcd.erl", ref: "79d04a775e4488b0eb6e5e07a8c0bf4803adb997"},
      {:hackney, "~> 1.5.0"},
      {:exrm, "~> 1.0"},
      {:dialyze, "~> 0.2.0", only: :dev},
      {:earmark, "~> 0.2", only: :dev},
      {:ex_doc, "~> 0.11", only: :dev},
      {:excoveralls, "~> 0.5", only: :test},
      {:eunit_formatters, "~> 0.3.0", only: :test}
    ]
  end

  # Aliases are shortcut or tasks specific to the current project.
  # For example, to create, migrate and run the seeds file at once:
  #
  #     $ mix ecto.setup
  #
  # See the documentation for `Mix` for more info on aliases.
  defp aliases do
    ["ecto.setup": ["ecto.create", "ecto.migrate", "run priv/repo/seeds.exs"],
     "ecto.reset": ["ecto.drop", "ecto.setup"]]
  end

  defp erlc_paths(:test), do: ["test/erlang"] ++ erlc_paths(:common)
  defp erlc_paths(_), do: ["src"]

  defp erlc_options(:test), do: [{:d, :TEST}]
  defp erlc_options(:dev), do: [:debug_info]
  defp erlc_options(_), do: []
end
