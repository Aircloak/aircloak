defmodule Air.Service.Export do
  alias Air.Repo

  import Ecto.Query, only: [where: 2]

  def reduce_while(user, initial, reducer) do
    Repo.transaction(
      fn ->
        user
        |> export_stream()
        |> Enum.reduce_while(initial, reducer)
      end,
      timeout: :timer.hours(1)
    )
  end

  defp export_stream(user) do
    Stream.concat([
      ["{"],
      [~s("user":), encode(user), ","],
      [~s("audit_logs": [)],
      audit_logs(user),
      ["]}"]
    ])
  end

  defp audit_logs(user) do
    Air.Schemas.AuditLog
    |> where(user_id: ^user.id)
    |> Repo.stream()
    |> Stream.map(&encode/1)
    |> Stream.intersperse(",")
  end

  defp encode(schema) do
    schema
    |> Map.from_struct()
    |> Map.drop([:__meta__, :groups, :user_id])
    |> Enum.reject(&match?({_, %Ecto.Association.NotLoaded{}}, &1))
    |> Enum.into(%{})
    |> Poison.encode!()
  end
end
