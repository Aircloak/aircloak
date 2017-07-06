defmodule Air.Service.Query.Result do
  @moduledoc "Helper functions for working with results."


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc """
  Extracts buckets for the desired row range from the given chunks.

  It is assumed that the chunks are already sorted in the ascending order by the offset field.
  """
  @spec buckets([Air.Schemas.ResultChunk.decoded_chunk], non_neg_integer, pos_integer) :: [map]
  def buckets(chunks, from, count), do:
    chunks
    |> positioned_buckets_stream()
    |> Stream.drop_while(&(&1.from + &1.count - 1 < from))
    |> Stream.take_while(&(&1.from <= from + count - 1))
    |> Enum.map(&to_bucket(&1, from, count))


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp to_bucket(bucket_data, from, count) do
    desired_to = from + count - 1
    bucket_to = bucket_data.from + bucket_data.count - 1
    discard_before = max(0, from - bucket_data.from)
    discard_after = max(0, bucket_to - desired_to)
    adjusted_row_count = Map.fetch!(bucket_data.bucket, "occurrences") - (discard_before + discard_after)
    %{bucket_data.bucket | "occurrences" => adjusted_row_count}
  end

  defp positioned_buckets_stream(chunks) do
    Stream.resource(
      fn -> bucket_iterator(chunks) end,
      fn(state) ->
        case next_bucket(state) do
          {bucket, state} -> {[bucket], state}
          nil -> {:halt, state}
        end
      end,
      fn(_) -> :ok end
    )
  end

  defp bucket_iterator(chunks), do:
    %{chunks: chunks, current_row: top_row(chunks)}

  defp top_row([]), do: nil
  defp top_row([first_chunk | _]), do: first_chunk.offset

  defp next_bucket(%{chunks: []}), do:
    nil
  defp next_bucket(%{chunks: [chunk | other_chunks]} = state) do
    case next_bucket_from_chunk(chunk) do
      nil ->
        next_bucket(%{state | chunks: other_chunks})

      {bucket, new_chunk} ->
        {
          %{from: chunk.offset, count: Map.fetch!(bucket, "occurrences"), bucket: bucket},
          %{state | chunks: [new_chunk | other_chunks]}
        }
    end
  end

  defp next_bucket_from_chunk(%{buckets: []}), do:
    nil
  defp next_bucket_from_chunk(%{buckets: [bucket | other_buckets]} = chunk), do:
    {
      bucket,
      %{chunk | buckets: other_buckets, offset: chunk.offset + Map.fetch!(bucket, "occurrences")}
    }
end
