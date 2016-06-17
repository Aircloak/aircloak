defmodule Cloak.SqlQuery.Compiler.Test do
  use ExUnit.Case, async: true

  alias Cloak.SqlQuery.Compiler
  alias Cloak.SqlQuery.Parser
  alias Timex.{DateTime, Timezone}

  test "casts timestamp where conditions" do
    data_source = %{tables: %{table: %{
      columns: [{"column", :timestamp}]
    }}}
    query = Parser.parse!("select * from table where column > '2015-01-01'")

    {:ok, result} = Compiler.compile(data_source, query)
    assert result[:where] ==
      [{:comparison, "column", :>, %DateTime{year: 2015, month: 1, day: 1, timezone: Timezone.get(:utc)}}]
  end
end
