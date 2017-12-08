defmodule AircloakCI.Emoji do
  @moduledoc "Module for retrieving emojis for CI notifications."


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Returns a random happy emoji."
  @spec happy() :: String.t
  def happy(), do: Enum.random(["💯", "👍", "😊", "❤️", "🎉", "👏"])

  @doc "Returns a random sad emoji."
  @spec sad() :: String.t
  def sad(), do: Enum.random(["😞", "😢", "😟", "💔", "👿", "🔥"])
end
