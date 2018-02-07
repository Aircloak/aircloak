defmodule Cloak.Sql.ParseError do
  @moduledoc "An error that occurred while parsing the query."
  defexception message: "Error while parsing query", source_location: nil
end
