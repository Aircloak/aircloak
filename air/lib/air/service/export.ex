defmodule Air.Service.Export do
  alias Air.Repo

  import Ecto.Query, only: [where: 2, from: 2]

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
      [~s("user":), prepare_user(user), ","],
      [~s("audit_logs": [)],
      audit_logs(user),
      [~s(], "views": [)],
      views(user),
      [~s(], "queries": [)],
      queries(user),
      [~s(], "result_chunks": [)],
      result_chunks(user),
      ["]}"]
    ])
  end

  defp prepare_user(user) do
    user = Repo.preload(user, :groups)

    user
    |> Map.put(:groups, Enum.map(user.groups, & &1.name))
    |> encode()
  end

  defp views(user), do: Air.Schemas.View |> where(user_id: ^user.id) |> stream()

  defp audit_logs(user), do: Air.Schemas.AuditLog |> where(user_id: ^user.id) |> stream()

  defp queries(user), do: Air.Schemas.Query |> where(user_id: ^user.id) |> stream()

  defp result_chunks(user) do
    from(
      chunk in Air.Schemas.ResultChunk,
      join: query in assoc(chunk, :query),
      where: query.user_id == ^user.id,
      select: chunk
    )
    |> stream(fn chunk -> %{chunk | encoded_data: :zlib.gunzip(chunk.encoded_data)} end)
  end

  defp stream(queryable, preprocessor \\ & &1),
    do: queryable |> Repo.stream() |> Stream.map(preprocessor) |> Stream.map(&encode/1) |> Stream.intersperse(",")

  defp encode(schema) do
    schema
    |> Map.from_struct()
    |> Map.drop([:__meta__, :user_id])
    |> Enum.reject(&match?({_, %Ecto.Association.NotLoaded{}}, &1))
    |> Enum.into(%{})
    |> Poison.encode!()
  end
end
