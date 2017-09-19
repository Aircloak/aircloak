defmodule Air.Service.Query.ResultConverter do
  @moduledoc """
  Incremental background conversion of old results into the new format.

  Up until 2017 Q3, we stored all buckets directly into the `result` field of the `query` table. With new changes,
  buckets are split into chunks of 1000, encoded (json + gzip), and stored into the `result_chunk` table. This
  module starts a transient worker which finds old style results and converts them into the new format.

  The conversion is done in this way, instead of a more traditional migration, because it can take quite long when
  there are many results with a large number of buckets. Once 2018 Q1 is released, we can safely remove this code.
  """

  use Aircloak.ChildSpec.Task, restart: :transient
  alias Air.Repo
  alias Air.Schemas.{Query, ResultChunk}
  require Logger
  import Ecto.Query


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Starts the converter process."
  @spec start_link :: {:ok, pid}
  def start_link(), do:
    Task.start_link(&convert_all_results/0)


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  @doc false
  def convert_all_results() do
    converted_count = convert_results(0)
    if converted_count > 0, do: Logger.info("converted #{converted_count} queries")
    converted_count
  end

  defp convert_results(converted_count) do
    case old_style_result() do
      nil ->
        converted_count

      %Query{} = query ->
        Logger.info("converting query results for #{query.id}")
        num_chunks = convert_query_result(query)
        # Naive heuristics -> the sleep time is proportional to the length of chunks. The more chunks we have, the
        # longer the processing was, so we'll let the server breathe for a while in case some other work piled up.
        :timer.sleep(min(num_chunks * 20, :timer.seconds(10)))
        convert_results(converted_count + 1)
    end
  end

  defp old_style_result(), do:
    Repo.one(
      from q in Query,
        select: q,
        where: fragment("? \\? 'rows'", q.result),
        order_by: [desc: q.inserted_at],
        limit: 1
    )

  defp convert_query_result(query) do
    chunks = make_chunks(query)

    {:ok, :ok} =
      Repo.transaction(fn ->
        Repo.insert_all(
          ResultChunk,
          Enum.map(chunks, &%{query_id: query.id, index: &1.index, encoded_data: &1.encoded_data})
        )

        query
        |> Query.changeset(%{result: Map.delete(query.result, "rows")})
        |> Repo.update!()

        :ok
      end)

    Logger.info("converted results for query #{query.id}")
    length(chunks)
  end

  defp make_chunks(query), do:
    (Map.fetch!(query.result, "rows") || [])
    |> Stream.chunk(1000, 1000, [])
    |> Stream.with_index()
    |> Enum.map(&encode_chunk/1)

  defp encode_chunk({rows, index}), do:
    %{
      index: index,
      encoded_data: rows |> :jiffy.encode([:use_nil]) |> :zlib.gzip()
    }
end
