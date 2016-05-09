defmodule Mix.Tasks.Compile.LuaLibraries do
  @shortdoc "Compiles lua libraries"
  @moduledoc """
  Compiles lua libraries.

  This module generates the `Air.TaskLibrary` module based on the contents of the
  `lua_libraries` folder. If you need to add another library, simply place it
  in that folder, and it the module will be recompiled.

  The namespace of a library is deduced from the camelized file name. For example,
  if the file is called 'date_time.lua', then the namespace will be `Aircloak.DateTime`.
  It is your responsibility to create the corresponding global table in the source
  code, and define function in that table:

  ```lua
  Aircloak.DateTime = {}

  function Aircloak.DateTime.day(time)
    ...
  end
  ```
  """
  use Mix.Task

  # Mix.Task behaviour is not in PLT since Mix is not a runtime dep, so we disable the warning
  @dialyzer :no_undefined_callbacks

  @lua_libraries_folder "lua_libraries"
  @generated_module Air.TaskLibrary

  @doc false
  def run(_args) do
    if Mix.Utils.stale?([__ENV__.file, @lua_libraries_folder | library_files()], [beam_path()]) do
      # Remove traces of the old module and unload it. Otherwise, we'll get a warning that we're
      # redefining the module
      File.rm_rf!(beam_path())
      :code.purge(@generated_module)
      :code.soft_purge(@generated_module)
      :code.delete(@generated_module)

      [{@generated_module, bytecode}] = Code.compile_quoted(module_ast())
      File.write!(beam_path(), bytecode)

      Mix.shell.info("Compiled #{@lua_libraries_folder}/")
    end
    :ok
  end

  defp module_ast() do
    quote location: :keep, bind_quoted: [libraries: Macro.escape(libraries()), module: @generated_module] do
      defmodule module do
        @moduledoc """
        Interface for fetching Aircloak task libraries.

        This module exposes various functions for working with Aircloak libraries. These
        functions are needed by the task runner to embed needed libraries, as well as
        the task editor to provide completion hints.
        """

        @all_completions [
          %{text: "report_property(label, string)", displayText: "report_property"},
          %{text: "get_user_tables()", displayText: "get_user_tables"},
          %{text: "to_date(timestamp)", displayText: "to_date"},
          %{text: "task_time"}
        ] ++ Enum.flat_map(libraries, &(&1.completions))

        @doc "Returns all possible completions."
        @spec completions() :: [%{text: String.t, displayText: String.t}]
        def completions(), do: @all_completions

        @all_libraries Enum.map(libraries, &({&1.namespace, {&1.filename, &1.code}})) |> Enum.into(%{})

        @doc "Returns all Aircloak dependencies required by the given code."
        @spec dependencies(String.t) :: [%{name: String.t, code: String.t}]
        def dependencies(code) do
          deps =
            for [namespace] <- Regex.scan(~r/(?'namespace'(\w+\.)+)/m, code, capture: ["namespace"]),
                filtered_namespace <- [String.replace(namespace, ~r/\.$/, "")],
                {filename, code} <- [@all_libraries[filtered_namespace]] do
              %{
                name: filename,
                code: code
              }
            end

          case deps do
            [] -> []
            [_|_] ->
              # We have some deps, so we also add the code which creates the top-level global table.
              [%{name: "aircloak.lua", code: "Aircloak = {}"} | deps]
          end
        end
      end
    end
  end

  defp libraries() do
    for library <- library_files() do
      # Read the code, and replace comments. Note that we're keeping empty lines, to get matching line numbers
      # in exception reports.
      code =
        File.read!(library)
        |> String.replace(~r/--.*$/m, "")

      # Finds all function declarations in the file. Using regex is hacky and not 100% correct, but does
      # the job.
      functions = Regex.scan(
        ~r/^function (?'text'(?'displayText'(\w|\.)+)\(.*\))/m,
        code,
        capture: ["displayText", "text"]
      )

      filename = Path.basename(library, ".lua")
      %{
        filename: "#{filename}.lua",
        namespace: "Aircloak.#{Inflex.camelize(filename)}",
        code: code,
        completions:
          (for [display_text, text] <- functions, do: %{text: text, displayText: display_text})
      }
    end
  end

  defp library_files() do
    Path.wildcard("#{@lua_libraries_folder}/*.lua")
  end

  defp beam_path() do
    Path.join(Mix.Project.compile_path(Mix.Project.config), "#{@generated_module}.beam")
  end
end
