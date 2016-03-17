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
        eunit: :test
      ],
      eunit_options: [
        :no_tty,
        {:report, {:eunit_progress, [:colored]}}
      ]
    ]
  end

  def application do
    [
      applications: [:logger, :lager, :webmachine, :ej, :gproc, :pgsql],
      mod: {Cloak, []}
    ]
  end

  defp deps do
    [
      {:aircloak_mix_tasks, path: "../common/mix_tasks"},
      {:lager, "~> 3.0"},
      {:webmachine, github: "basho/webmachine", tag: "1.10.6"},
      {:ej, github: "seth/ej"},
      {:gproc, "~> 0.5.0"},
      {:pgsql, github: "semiocast/pgsql"},
      {:eunit_formatters, "~> 0.3.0", only: :test}
    ]
  end

  defp erlc_options(:test), do: [:debug_info, {:d, :TEST}]
  defp erlc_options(:dev), do: [:debug_info]
  defp erlc_options(:prod), do: []

  defp erlc_paths(:test), do: ["test/erlang/eunit", "src"]
  defp erlc_paths(:dev), do: ["src"]
  defp erlc_paths(:prod), do: ["src"]
end
