defmodule Mix.Tasks.Eunit do
  @moduledoc """
  Mix task for running EUnit tests.

  You can start the task from the command line with `mix eunit`
  """
  use Mix.Task

  @shortdoc "Runs eunit tests"
  @recursive true

  @doc false
  def run(args) do
    Mix.Task.run("loadpaths", args)
    Mix.Task.run("app.start", args)

    Application.ensure_all_started(:eunit)
    project_config = Mix.Project.config
    :eunit.test({:application, project_config[:app]}, project_config[:eunit_options] || [])
  end
end
