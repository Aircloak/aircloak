defmodule IntegrationTests.Mixfile do
  use Mix.Project

  def project do
    [
      app: :integration_tests,
      version: "0.1.0",
      elixir: "~> 1.3",
      build_embedded: Mix.env == :prod,
      start_permanent: Mix.env == :prod,
      deps: deps(),
      deps_path: Path.join(["deps", otp_version(), elixir_version()]),
      elixirc_paths: elixirc_paths(Mix.env),
      preferred_cli_env: ["deps.get": :test],
    ]
  end

  def application do
    [
      mod: {IntegrationTest.Manager, []},
      # We're not starting these apps automatically, since we need to do additional setup before they are
      # started. However, we're specifying them as included to ensure they are loaded and configured.
      included_applications: [:cloak, :air, :central],
      extra_applications: [:logger, :odbc],
    ]
  end

  defp deps do
    [
      {:cloak, path: "../cloak", runtime: false},
      {:air, path: "../air", runtime: false},
      {:central, path: "../central", runtime: false},
      # resolving clashed dependencies
      {:mariaex, "~> 0.8", override: true},
      {:ecto, "~> 2.2.0", override: true},
      {:postgrex, "~> 0.13.0", override: true},
      {:poison, github: "cristianberneanu/poison", override: true},
      {:httpoison, "~> 0.12.0", override: true},
    ]
  end

  defp elixirc_paths(:test), do: ["lib", "test/support"]
  defp elixirc_paths(_), do: ["lib"]

  defp otp_version(), do:
    [:code.root_dir(), "releases", :erlang.system_info(:otp_release), "OTP_VERSION"]
    |> Path.join()
    |> File.read!()
    |> String.trim("\n")

  defp elixir_version(), do:
    System.version()
end
