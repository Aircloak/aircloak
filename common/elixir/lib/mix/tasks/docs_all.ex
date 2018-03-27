defmodule Mix.Tasks.Docs.All do
  @shortdoc "Generates Elixir and Erlang documentation"
  @moduledoc """
  Generates Elixir and Erlang documentation.

  The documentation is generated in `doc/elixir` and `doc/erlang` folders.
  """
  use Mix.Task

  # Mix.Task behaviour is not in PLT since Mix is not a runtime dep, so we disable the warning
  @dialyzer :no_undefined_callbacks

  @recursive true

  @impl Mix.Task
  def run(_args) do
    Mix.Task.run("docs", ["--output", "doc/elixir"])
    :ok = :edoc.application(Mix.Project.config()[:app], '.', preprocess: true, dir: 'doc/erlang')
    Mix.shell().info([:green, "Erlang docs can be found at doc/erlang/index.html"])
  end
end
