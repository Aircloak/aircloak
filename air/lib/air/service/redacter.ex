defmodule Air.Service.Redacter do
  @moduledoc "Services for redacting sensitive information."


  #-----------------------------------------------------------------------------------------------------------
  # Whitelist
  #-----------------------------------------------------------------------------------------------------------

  # These are terms that will not be filtered by the redacter.
  # They constitute common Aircloak phrases that are known to be safe.
  @safe_terms [
    "select or show",
    # Common syntax
    "GROUP BY", "WHERE", "SELECT", "ORDER BY", "HAVING", "SHOW", "LIMIT", 
    "JOIN", "CROSS JOIN", "INNER JOIN", "LEFT JOIN", "RIGHT JOIN", "OUTER JOIN",
    # Supported types
    "text", "integer", "real", "boolean", "datetime", "date", "time", "uuid", "unknown",
    # Common function names
    "count", "count_noise",
    "sum", "sum_noise",
    "min", "max", "median",
    "avg", "stddev", "avg_noise", "stddev_noise",
    "hour", "minute", "second", "year", "month", "day", "weekday",
    "floor", "ceil", "ceiling",
    "round", "trunc",
    "abs", "sqrt", "div", "mod", "%", "pow", "^", "+", "-", "*", "/",
    "length", "lower", "lcase", "upper", "ucase", "left", "right",
    "btrim", "ltrim", "rtrim", "substring", "substring_for",
    "||", "concat", "hex",
    "extract_match", "extract_matches",
  ]
  |> Enum.map(&String.downcase/1)
  |> Enum.map(& "`#{&1}`")


  #-----------------------------------------------------------------------------------------------------------
  # API functions
  #-----------------------------------------------------------------------------------------------------------

  @doc "Filters sensitive column and table names from error messages so they can be communicated to Aircloak"
  @spec filter_query_error(String.t) :: String.t
  def filter_query_error(error), do: Regex.replace(~r/`.*?`/, error, &redaction_checker/1, global: true)


  #-----------------------------------------------------------------------------------------------------------
  # Internal functions
  #-----------------------------------------------------------------------------------------------------------

  defp redaction_checker(string) do
    if String.downcase(string) in @safe_terms do
      string
    else
      "`redacted`"
    end
  end
end
