defmodule Cloak.DataSource.Isolators.Cache.Test do
  use ExUnit.Case, async: true
  alias Cloak.DataSource.Isolators.Cache

  test "computes isolated for known columns" do
    known_columns = ~w(col1 col2 col3)
    provider = new_cache_provider(known_columns)
    {:ok, cache} = Cache.start_link(provider.cache_opts)

    Enum.each(
      known_columns,
      &assert(Cache.isolates_users?(cache, provider.data_source, provider.table_name, &1) == {:isolated, &1})
    )
  end

  defp new_cache_provider(column_names) do
    data_source = %{name: make_ref()}
    table_name = make_ref()
    columns = Enum.map(column_names, &{data_source.name, table_name, &1})

    {:ok, provider} = Agent.start_link(fn -> columns end)

    cache_opts = [
      columns_provider: fn -> Agent.get(provider, & &1) end,
      compute_isolation_fun: fn {_data_source_name, _table_name, column_name} -> {:isolated, column_name} end,
      registered?: false
    ]

    %{
      provider: provider,
      cache_opts: cache_opts,
      data_source: data_source,
      table_name: table_name
    }
  end
end
