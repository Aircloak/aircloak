defmodule Cloak.SqlQuery.Compiler.Test do
  use ExUnit.Case, async: true

  alias Cloak.SqlQuery.Compiler
  alias Cloak.SqlQuery.Parsers.Token
  alias Timex.{DateTime, Timezone}

  test "casts timestamp where conditions" do
    data_source = %{
      tables: %{table: %{columns: [{"column", :timestamp}]}}
    }
    literal = %Token{category: :constant, value: %{type: :string, value: "2015-01-01"}}
    query = %{
      select: [:*],
      from: "table",
      where: [{:comparison, "column", :>, literal}]
    }

    {:ok, result} = Compiler.compile(data_source, query)
    assert result[:where] ==
      [{:comparison, "column", :>, %DateTime{year: 2015, month: 1, day: 1, timezone: Timezone.get(:utc)}}]
  end
end
