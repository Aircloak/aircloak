defmodule Air.Service.Redacter do
  @moduledoc "Services for redacting sensitive information."


  #-----------------------------------------------------------------------------------------------------------
  # API functions
  #-----------------------------------------------------------------------------------------------------------

  @doc "Filters sensitive column and table names from error messages so they can be communicated to Aircloak"
  @spec filter_query_error(String.t) :: String.t
  def filter_query_error(error), do: String.replace(error, ~r/`.*`/, "`redacted`", global: true)
end
