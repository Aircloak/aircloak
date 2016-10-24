defmodule Cloak.Features do
  @moduledoc "Contains functions for checking if configurable features are enabled."

  require Aircloak.DeployConfig

  @opaque t :: %{}

  @doc "Returns the feature list that should be used for all non-test purposes."
  @spec from_config :: t
  def from_config, do: Aircloak.DeployConfig.get("features", %{})

  @doc "Returns true if the given feature list has the given feature, false otherwise."
  @spec has?(t, atom) :: boolean
  def has?(features, feature), do: features[to_string(feature)]
end
