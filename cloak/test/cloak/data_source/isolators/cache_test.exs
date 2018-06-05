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

  test "isolated error for unknown columns" do
    known_columns = ~w(col1 col2 col3)
    provider = new_cache_provider(known_columns)
    {:ok, cache} = Cache.start_link(provider.cache_opts)

    assert_raise(RuntimeError, fn ->
      Cache.isolates_users?(cache, provider.data_source, provider.table_name, "unknown col")
    end)
  end

  test "prioritizing on demand" do
    known_columns = ~w(col1 col2 col3)

    provider =
      new_cache_provider(
        known_columns,
        compute_isolation_fun:
          compute_isolation_fun(%{
            "col1" => fn -> Process.sleep(200) end,
            "col2" => fn -> Process.sleep(:infinity) end
          })
      )

    {:ok, cache} = Cache.start_link(provider.cache_opts)
    assert Cache.isolates_users?(cache, provider.data_source, provider.table_name, "col3") == {:isolated, "col3"}
  end

  defp new_cache_provider(column_names, opts \\ []) do
    data_source = %{name: inspect(make_ref())}
    table_name = inspect(make_ref())
    columns = Enum.map(column_names, &{data_source.name, table_name, &1})

    {:ok, provider} = Agent.start_link(fn -> columns end)

    cache_opts =
      Keyword.merge(
        [
          columns_provider: columns_provider(provider),
          compute_isolation_fun: compute_isolation_fun(),
          registered?: false
        ],
        opts
      )

    %{
      provider: provider,
      cache_opts: cache_opts,
      data_source: data_source,
      table_name: table_name
    }
  end

  defp columns_provider(provider), do: fn -> Agent.get(provider, & &1) end

  defp compute_isolation_fun(map \\ %{}) do
    fn {_data_source_name, _table_name, column_name} ->
      with {:ok, fun} <- Map.fetch(map, column_name), do: fun.()
      {:isolated, column_name}
    end
  end
end
