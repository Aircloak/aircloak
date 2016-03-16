defmodule Mix.Tasks.Eunit do
  @shortdoc "Runs eunit tests"
  @moduledoc """
  Mix task for running EUnit tests.

  You can start the task from the command line with `mix eunit`
  """
  use Mix.Task

  # Mix.Task behaviour is not in PLT since Mix is not a runtime dep, so we disable the warning
  @dialyzer :no_undefined_callbacks

  @recursive true

  @doc false
  def run(args) do
    project_config = Mix.Project.config

    # It seems that modules are already loaded at this point, so eunit doesn't pick up new versions if the
    # code is changed. To fix this, we're reloading all modules of this app.
    for {module, path} <- :code.all_loaded,
        String.starts_with?(to_string(path), "#{Mix.Project.build_path}/lib/#{project_config[:app]}/ebin/")
    do
      :code.purge(module)
      :code.soft_purge(module)
      {:module, _} = :code.load_file(module)
    end

    Mix.Task.run("loadpaths", args)
    Mix.Task.run("app.start", args)

    Application.ensure_all_started(:eunit)
    :eunit.test({:application, project_config[:app]}, project_config[:eunit_options] || [])
  end
end
