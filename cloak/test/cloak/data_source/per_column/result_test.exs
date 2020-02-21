defmodule Cloak.DataSource.PerColumn.Result.Test do
  use ExUnit.Case, async: true
  use ExUnitProperties
  alias Cloak.DataSource.PerColumn.Result
  import Cloak.Test.Generators

  property "we can decrypt anything we encrypt" do
    check all(result <- result()) do
      assert result ==
               result |> Result.encrypt() |> Result.decrypt()
    end
  end

  defp result() do
    gen all(
          descriptor <- binary(),
          type <- one_of([Cloak.DataSource.Isolators.Cache]),
          result <- integer(),
          expires <- naive_datetime()
        ) do
      Result.new(descriptor, type, result, expires)
    end
  end
end
