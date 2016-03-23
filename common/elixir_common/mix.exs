defmodule Aircloak.ElixirCommon.Mixfile do
  use Mix.Project

  def project do
    [
      app: :aircloak_elixir_common,
      version: "0.0.1",
      elixir: "~> 1.2",
      build_embedded: Mix.env == :prod,
      start_permanent: Mix.env == :prod,
      deps: deps
    ]
  end

  def application do
    [applications: [:logger]]
  end

  defp deps do
    [
      {:protobuffs, github: "basho/erlang_protobuffs", tag: "0.8.2"}
    ]
  end
end
