defmodule Mix.Tasks.Proper do
  @shortdoc "Runs PropEr tests"
  @moduledoc """
  Mix task for running PropEr tests.

  You can start the task from the command line with `mix proper`. All modules which
  export the `proper` attribute will be tested.

  To test a single module, you can run `mix proper --module target_module`
  To test the specific proper level of modules, you can use the `--level target_level` option.
  """
  use Mix.Task

  # Mix.Task behaviour is not in PLT since Mix is not a runtime dep, so we disable the warning
  @dialyzer :no_undefined_callbacks

  @recursive true

  @doc false
  def run(args) do
    project_config = Mix.Project.config

    # It seems that modules are already loaded at this point, so proper doesn't pick up new versions if the
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

    {options, _, _} = OptionParser.parse(args)
    modules = case options[:module] do
      nil -> all_proper_modules(project_config, String.to_atom(options[:level] || "all"))
      module -> [:"#{module}_test"]
    end

    for module <- modules do
      case :proper.module(module) do
        [] -> :ok
        _ -> Mix.raise("PropEr error")
      end
    end

    :ok
  end

  defp all_proper_modules(project_config, level) do
    for beam_name <- Path.wildcard("#{Mix.Project.build_path}/lib/#{project_config[:app]}/ebin/*.beam"),
        module_name = Path.basename(beam_name, ".beam"),
        not String.starts_with?(module_name, "Elixir"),
        module = String.to_atom(module_name),
        Enum.any?(Keyword.keys(module.module_info(:exports)), &String.starts_with?(to_string(&1), "prop_")),
        include?(module.module_info(:attributes)[:proper], level),
        do: module
  end

  defp include?(nil, _), do: false
  defp include?(_, :all), do: true
  defp include?([level], level), do: true
  defp include?(_, _), do: false
end
