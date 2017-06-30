defmodule Cloak.Test.MongoHelpers do
  @moduledoc false

  alias Cloak.Query.Runner

  defmacro assert_query(context, query, parameters \\ [], expected_response) do
    quote do
      Runner.start("1", unquote(context).data_source, unquote(query), unquote(parameters), %{}, {:process, self()})
      response = receive do
        {:result, response} -> response
      end
      assert unquote(expected_response) = response
    end
  end
end
