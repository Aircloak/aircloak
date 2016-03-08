defmodule Mix.Tasks.Eunit do
  use Mix.Task

  @shortdoc "Runs eunit tests"
  @recursive true

  def run(args) do
    Mix.Task.run("loadpaths", args)
    Mix.Task.run("app.start", args)

    Application.ensure_all_started(:eunit)
    project_config = Mix.Project.config
    :eunit.test({:application, project_config[:app]}, project_config[:eunit_options] || [])
  end
end
