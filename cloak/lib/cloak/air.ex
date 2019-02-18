defmodule Cloak.Air do
  @moduledoc "Functions for registering and accessing properties of the connected air instance."

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Registers the air instance."
  @spec register_air(String.t()) :: :ok
  def register_air(air_name), do: Application.put_env(:cloak, :air_name, air_name)

  @doc "Unregisters the air instance."
  @spec unregister_air() :: :ok
  def unregister_air(), do: Application.delete_env(:cloak, :air_name)

  @doc "Returns the name of the registered air instance, or an error if the air is not connected."
  @spec name :: {:ok, String.t()} | {:error, String.t()}
  def name() do
    case Application.get_env(:cloak, :air_name) do
      nil -> {:error, "air is not connected"}
      name -> {:ok, name}
    end
  end
end
