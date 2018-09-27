defmodule Aircloak.Version do
  @moduledoc "Module to make it easier to work with aircloak specific versions and release names."

  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @doc "Reads out the version of an OTP application from its spec"
  @spec for_app(atom) :: String.t()
  def for_app(application_name),
    do:
      Application.spec(application_name)
      |> Keyword.get(:vsn)
      |> List.to_string()
end
