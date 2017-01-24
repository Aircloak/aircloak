defmodule Air.Service.Settings do
  @moduledoc "Services for reading and writing Air-wide options."


  #-----------------------------------------------------------------------------------------------------------
  # API functions
  #-----------------------------------------------------------------------------------------------------------

  @doc "Returns the current version of the settings."
  @spec read() :: Air.Settings.t
  def read(), do: parse(latest_settings() || %{})

  @doc "Updates the specified fields in the settings."
  @spec update(%{optional(atom) => any()}) :: :ok
  def update(settings), do:
    read() |> Map.merge(settings) |> write()


  #-----------------------------------------------------------------------------------------------------------
  # Internal functions
  #-----------------------------------------------------------------------------------------------------------

  defp write(%Air.Settings{query_retention_days: days}) do
    %Air.Schemas.Settings{}
    |> Air.Schemas.Settings.changeset(%{query_retention_days: serialize_retention_days(days)})
    |> Air.Repo.insert!()
  end

  defp parse(data) do
    %Air.Settings{query_retention_days: Map.get(data, :query_retention_days) |> unserialize_retention_days()}
  end

  defp latest_settings() do
    import Ecto.Query
    Air.Schemas.Settings |> last() |> Air.Repo.one()
  end

  defp unserialize_retention_days(nil), do: :unlimited
  defp unserialize_retention_days(days), do: days

  defp serialize_retention_days(:unlimited), do: nil
  defp serialize_retention_days(days), do: days
end
