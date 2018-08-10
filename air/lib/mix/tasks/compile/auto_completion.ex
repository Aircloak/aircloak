defmodule Mix.Tasks.Compile.AutoCompletion do
  @shortdoc "Compiles auto-completion for the web editor based on supported functions."
  @moduledoc "Compiles auto-completion."
  use Mix.Task
  require Logger

  # Mix.Task behaviour is not in PLT since Mix is not a runtime dep, so we disable the warning
  @dialyzer :no_undefined_callbacks

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @impl Mix.Task
  def run(_args) do
    Logger.info("Compiling auto-completions")
    compile_auto_completions()

    :ok
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp compile_auto_completions() do
    function_names =
      Aircloak.Functions.function_spec()
      |> Enum.flat_map(&create_function_definitions/1)
      |> Enum.map(&"  \"#{&1}\"")
      |> Enum.join(",\n")

    auto_completions = """
    export const FUNCTION_KEYWORDS=[
    #{function_names}
    ];
    """

    File.write!("assets/js/code_editor/function_completion_keywords.js", auto_completions)
  end

  defp create_function_definitions({{:cast, target}, _}), do: ["cast(<column> to #{target})"]

  defp create_function_definitions({{:bucket, alignment}, _}),
    do: ["bucket(<column> by <constant> align #{alignment})"]

  defp create_function_definitions({function, attributes}),
    do:
      attributes
      |> Map.get(:type_specs)
      |> Map.keys()
      |> Enum.map(&"#{function}(#{arguments_from_spec(&1)})")

  defp arguments_from_spec({:many1, _} = t), do: arguments_from_spec([t])
  defp arguments_from_spec(types), do: types |> Enum.map(&argument_from_spec(&1, :top)) |> Enum.join(", ")

  defp argument_from_spec({:constant, type}, level),
    do: wrap(argument_from_spec(type, :nested) <> " constant", level)

  defp argument_from_spec({:many1, type}, level), do: wrap(argument_from_spec(type, :nested) <> ", ...", level)
  defp argument_from_spec({:optional, type}, level), do: "[#{wrap(argument_from_spec(type, :nested), level)}]"

  defp argument_from_spec({:or, alternatives}, level),
    do:
      argument_from_spec(
        alternatives
        |> Enum.map(&argument_from_spec(&1, :nested))
        |> Enum.join("|"),
        level
      )

  defp argument_from_spec(type, level), do: wrap(type, level)

  defp wrap(value, :top), do: "<#{value}>"
  defp wrap(value, _), do: to_string(value)
end
