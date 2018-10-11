defmodule Cloak.Sql.Parser.ParseError do
  @moduledoc "An error that occurred while parsing the query."
  defexception message: "Error while parsing query", source_location: nil
end
