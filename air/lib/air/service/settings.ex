defmodule Air.Service.Settings do
  @moduledoc "Services for reading and writing Air-wide options."

  alias Air.Service.AuditLog


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Returns the current version of the settings."
  @spec read() :: Air.Settings.t
  def read(), do: parse(latest_settings() || %{})

  @doc "Updates the specified fields in the settings."
  @spec update(nil | User.t, %{optional(atom) => any()}) :: :ok
  def update(user, settings) do
    read() |> Map.merge(settings) |> write()
    AuditLog.log(user, "Updated settings", settings)
    :ok
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp write(%Air.Settings{query_retention_days: days, audit_log_enabled: audit_log_enabled}) do
    %Air.Schemas.Settings{}
    |> Air.Schemas.Settings.changeset(%{
      query_retention_days: serialize_retention_days(days),
      audit_log_enabled: audit_log_enabled,
    })
    |> Air.Repo.insert!()
  end

  defp parse(data) do
    %Air.Settings{
      query_retention_days: Map.get(data, :query_retention_days) |> unserialize_retention_days(),
      audit_log_enabled: Map.get(data, :audit_log_enabled, true),
    }
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
