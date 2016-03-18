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
        eunit: :test, proper: :test, "test.standard": :test, dialyze: :dev
      ],
      eunit_options: [
        :no_tty,
        {:report, {:eunit_progress, [:colored]}}
      ],
      aliases: aliases(Mix.env)
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
      {:aircloak_mix_tasks, path: "../common/mix_tasks"},
      {:lager, "~> 3.0"},
      {:lager_logger, "~> 1.0"},
      {:webmachine, github: "basho/webmachine", tag: "1.10.6"},
      {:ej, github: "seth/ej"},
      {:gproc, "~> 0.5.0"},
      {:pgsql, github: "semiocast/pgsql"},
      {:eunit_formatters, "~> 0.3.0", only: :test},
      {:proper, github: "matthiaskr/proper", only: :test, ref: "164663a7de18b0ce8d037b617afed0f97cac3de9"},
      {:dialyze, "~> 0.2.1", only: :dev}
    ]
  end

  defp applications(:test), do: common_applications()
  defp applications(:dev), do: [:os_mon | common_applications()]
  defp applications(:prod), do: [:os_mon | common_applications()]

  defp common_applications do
    [:logger, :lager_logger, :lager, :webmachine, :ej, :gproc, :pgsql]
  end

  defp erlc_options(:test), do: [:debug_info, {:d, :TEST}]
  defp erlc_options(:dev), do: [:debug_info]
  defp erlc_options(:prod), do: []

  defp erlc_paths(:test), do: ["test/erlang/eunit", "test/erlang/proper", "src"]
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
      "test.standard": ["test", "eunit", "proper --level simple"]
    ]
  end
  defp aliases(:prod), do: []
end
