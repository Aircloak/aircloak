defmodule Air.Service.Settings do
  def read(), do: parse(latest_settings() || %{})

  def update(settings) do
    read() |> Map.merge(settings) |> write()
  end

  defp write(%Air.Settings{query_retention_days: days}) do
    %Air.Schemas.Settings{}
    |> Air.Schemas.Settings.changeset(%{query_retention_days: serialize_retention_days(days)})
    |> Air.Repo.insert!()
  end

  defp parse(data) do
    %Air.Settings{query_retention_days: Map.get(data, :query_retention_days, :unlimited)}
  end

  defp latest_settings() do
    import Ecto.Query
    Air.Schemas.Settings |> last() |> Air.Repo.one()
  end

  defp serialize_retention_days(:unlimited), do: nil
  defp serialize_retention_days(days), do: days
end
