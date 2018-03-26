defmodule Mix.Tasks.Eunit do
  @shortdoc "Runs eunit tests"
  @moduledoc """
  Mix task for running EUnit tests.

  You can start the task from the command line with `mix eunit`
  To test a single module, you can run `mix eunit --module target_module`
  """
  use Mix.Task

  # Mix.Task behaviour is not in PLT since Mix is not a runtime dep, so we disable the warning
  @dialyzer :no_undefined_callbacks

  @recursive true

  @impl Mix.Task
  def run(args) do
    project_config = Mix.Project.config()

    Mix.Task.reenable("compile")
    Mix.Task.run("compile")

    # It seems that modules are already loaded at this point, so eunit doesn't pick up new versions if the
    # code is changed. To fix this, we're reloading all modules of this app.
    for {module, path} <- :code.all_loaded(),
        String.starts_with?(
          to_string(path),
          "#{Mix.Project.build_path()}/lib/#{project_config[:app]}/ebin/"
        ) do
      :code.purge(module)
      :code.soft_purge(module)
      {:module, _} = :code.load_file(module)
    end

    Mix.Task.run("loadpaths", args)
    Mix.Task.run("app.start", args)

    {options, _, _} = OptionParser.parse(args)

    test_target =
      case options[:module] do
        nil -> {:application, project_config[:app]}
        module -> {:module, String.to_atom(module)}
      end

    Application.ensure_all_started(:eunit)
    eunit_result = :eunit.test(test_target, project_config[:eunit_options] || [])

    # Exit hook which exits with non-zero status on errors
    System.at_exit(fn _ -> if eunit_result == :error, do: exit({:shutdown, 1}) end)

    :ok
  end
end
