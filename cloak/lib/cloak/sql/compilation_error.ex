defmodule Cloak.Sql.CompilationError do
  @moduledoc "An error that occurred while compiling the query."
  defexception message: "Error during compiling query", source_location: nil
end
