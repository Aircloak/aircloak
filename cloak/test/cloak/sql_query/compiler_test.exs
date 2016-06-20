defmodule Cloak.SqlQuery.Compiler.Test do
  use ExUnit.Case, async: true

  alias Cloak.SqlQuery.Compiler
  alias Cloak.SqlQuery.Parser
  alias Timex.{DateTime, Timezone}

  setup do
    {:ok, %{
      data_source: %{tables: %{table: %{
         columns: [{"column", :timestamp}]
      }}}
    }}
  end

  test "casts timestamp where conditions", %{data_source: data_source} do
    result = compile("select * from table where column > '2015-01-01'", data_source)

    assert result[:where] ==
      [{:comparison, "column", :>, %DateTime{year: 2015, month: 1, day: 1, timezone: Timezone.get(:utc)}}]
  end

  test "casts timestamp in `in` conditions", %{data_source: data_source} do
    result = compile("select * from table where column in ('2015-01-01', '2015-01-02')", data_source)

    assert [{:in, "column", times}] = result[:where]
    assert Enum.sort(times) == [
      %DateTime{year: 2015, month: 1, day: 1, timezone: Timezone.get(:utc)},
      %DateTime{year: 2015, month: 1, day: 2, timezone: Timezone.get(:utc)},
    ]
  end

  test "casts timestamp in negated conditions"

  defp compile(query_string, data_source) do
    query = Parser.parse!(query_string)
    {:ok, result} = Compiler.compile(data_source, query)
    result
  end
end
