defmodule Aircloak.Version do
  @moduledoc "Module to make it easier to work with aircloak specific versions and release names."

  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @doc "Reads out the version of an OTP application from its spec"
  @spec for_app(atom) :: Version.t()
  def for_app(application_name) do
    version_string =
      Application.spec(application_name)
      |> Keyword.get(:vsn)
      |> List.to_string()

    # Using apply to trick dialyzer, which thinks that `Version.parse!` will always fail.
    apply(Version, :parse!, [version_string])
  end

  @doc "Converts a version into a printable string representation"
  @spec to_string(Version.t()) :: String.t()
  def to_string(version), do: "#{version.major}.#{version.minor}.#{version.patch}"
end
