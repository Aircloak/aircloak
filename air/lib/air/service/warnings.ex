defmodule Air.Service.Warnings do
  @moduledoc "A service providing warnings about problems in the system"


  #-----------------------------------------------------------------------------------------------------------
  # API functions
  #-----------------------------------------------------------------------------------------------------------

  @doc "Whether or not there are any known problems"
  @spec known_problems?() :: boolean
  def known_problems?(), do: false
end
