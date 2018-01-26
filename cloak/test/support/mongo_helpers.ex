defmodule Cloak.Test.MongoHelpers do
  @moduledoc false

  alias Cloak.Query.Runner

  defmacro assert_query(context, query, expected_response) do
    quote do
      [first_response | other_responses] =
        ["3.0.0", "3.2.0", "3.4.0"] # mongo pipeline versions that we want to test against
        |> Enum.map(&Task.async(fn ->
          data_source = Map.put(unquote(context).data_source, :driver_info, &1)
          run_query!(data_source, unquote(query))
        end))
        |> Enum.map(&Task.await(&1, :timer.seconds(60)))
        |> Enum.map(&Map.drop(&1, [:execution_time, :features]))

      for other_response <- other_responses, do:
        assert(first_response == other_response)

      assert unquote(expected_response) = first_response
    end
  end

  def run_query!(data_source, query) do
    Runner.start("1", data_source, query, [], %{}, {:process, self()})
    receive do
      {:result, response} -> response
    end
  end
end
