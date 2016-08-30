defmodule PerfTest do
  alias Cloak.Test.DB

  @table_name "aircloak_perftest"

  def run(users_count_in_k) do
    db_setup()
    create_data(users_count_in_k)
  end

  defp db_setup() do
    IO.puts ">>> Creating perftest table ..."
    {:ok, _} = DB.execute("DROP TABLE IF EXISTS #{@table_name}", [])
    {:ok, _} = DB.execute("CREATE TABLE #{@table_name} (user_id TEXT, item TEXT, price INTEGER)", [])
    IO.puts ">>> Perftest table created succesfully."
    :ok
  end

  defp create_data(users_count_in_k) do
    IO.puts ">>> Creating test data: #{users_count_in_k} K users and #{users_count_in_k * 500.5} K rows ..."
    Enum.each(1 .. (users_count_in_k * 1_000), fn (user_index) ->
      row_count = rem(user_index, 1_000) + 1
      values =
        1 .. row_count
        |> Enum.map(fn (row_index) -> "('user_#{user_index}', 'item#{row_index}', #{row_index})" end)
        |> Enum.join(",")
      if rem(user_index, 100) == 1, do: IO.puts ">>> Creating data for users #{user_index} to #{user_index + 99}..."
      {:ok, _} = DB.execute("INSERT INTO #{@table_name} (user_id, item, price ) VALUES #{values}", [])
    end)
    IO.puts ">>> Test data created succesfully."
  end
end

users_count_in_k = case System.argv do
  [param] -> String.to_integer(param)
  _ -> 16 # 16 k users, ~8 M rows
end
PerfTest.run(users_count_in_k)
