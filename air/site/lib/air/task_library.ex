defmodule Air.TaskLibrary do
  @moduledoc """
  Interface for fetching Aircloak task libraries.

  This module exposes various functions for working with Aircloak libraries. These
  functions are mostly needed by task runner, to embed needed libraries, as well as
  the task editor, to provide completion hints.

  Internally, the module is generated at compile-time from libraries available in
  the `task_library` subfolder. If you need to add another library, simply place it
  in that folder, and it will be included in the module after recompilation.

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

  libraries =
    for library <- Path.wildcard("#{Path.rootname(__ENV__.file)}/*.lua") do
      @external_resource Path.relative_to_cwd(library)

      code = File.read!(library)

      # Finds all function declarations in the file. Using regex is hacky and not 100% correct, but does
      # the job.
      functions = Regex.scan(
            ~r/^function (?'displayText'(?'text'(\w|\.)+)\(.*\))/m,
            code,
            capture: ["displayText", "text"]
          )

      %{
        namespace: "Aircloak." <> (library |> Path.basename(".lua") |> Inflex.camelize()),
        code: code,
        completions:
          (for [display_text, text] <- functions, do: %{text: text, displayText: display_text})
      }
    end

  @all_completions [
    %{text: "report_property", displayText: "report_property(label, string)"},
    %{text: "get_user_tables()"},
    %{text: "to_date", displayText: "to_date(timestamp)"},
    %{text: "task_time"}
  ] ++ Enum.flat_map(libraries, &(&1.completions))

  @doc "Returns all possible completions."
  @spec completions() :: [%{text: String.t, displayText: String.t}]
  def completions(), do: @all_completions
end
