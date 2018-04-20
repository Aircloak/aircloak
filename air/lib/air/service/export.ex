defmodule Air.Service.Export do
  @moduledoc "Service module for creating an export of a user's data to comply with GDPR"

  alias Air.Repo

  import Ecto.Query, only: [where: 2, from: 2]

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc """
  Reduces over the stream of iolists representing the export of the given user's data as JSON. The reducing function
  should return `{:cont, accumulator}` to continue the reduction or `{:halt, accumulator}` to stop.
  """
  @spec reduce_while(Air.Schemas.User.t(), acc, (iolist, acc -> {:cont, acc} | {:halt, acc})) ::
          {:ok, acc} | {:error, :export_in_progress} | {:error, any}
        when acc: var
  def reduce_while(user, initial, reducer) do
    case Registry.register(__MODULE__, user.id, :in_progress) do
      {:ok, _} -> do_reduce_while(user, initial, reducer)
      {:error, _} -> {:error, :export_in_progress}
    end
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp do_reduce_while(user, initial, reducer) do
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
      [~s(], "api_tokens": [)],
      api_tokens(user),
      ["]}"]
    ])
  end

  defp prepare_user(user) do
    user = Repo.preload(user, groups: :data_sources)

    user
    |> Map.put(:groups, Enum.map(user.groups, &prepare_group/1))
    |> encode()
  end

  defp prepare_group(group) do
    %{name: group.name, data_sources: Enum.map(group.data_sources, &Map.take(&1, [:name, :id]))}
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

  defp api_tokens(user), do: Air.Schemas.ApiToken |> where(user_id: ^user.id) |> stream()

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

  # -------------------------------------------------------------------
  # Supervision tree
  # -------------------------------------------------------------------

  def child_spec(_arg), do: Aircloak.ChildSpec.registry(:unique, __MODULE__)
end
