defmodule Cloak.Sql.CompilationError do
  @moduledoc "An error that occurred while compiling the query."
  defexception message: "Error during compiling query", source_location: nil

  def message(%__MODULE__{message: message, source_location: nil}), do: message
  def message(%__MODULE__{message: message, source_location: {line, column}}), do:
    "#{message} At line #{line}, column #{column}."
end
