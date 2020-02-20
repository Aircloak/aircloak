defmodule Cloak.DataSource.PerColumn.Cache.Test do
  use ExUnit.Case, async: true
  alias Cloak.DataSource.PerColumn.{Cache, PersistentKeyValue, Descriptor, Result}

  setup_all do
    {:ok, _} = PersistentKeyValue.start_link(%{name: __MODULE__, persisted_cache_version: 1})
    :ok
  end

  describe ".value" do
    test "computes isolated for known columns" do
      known_columns = ~w(col1 col2 col3)
      provider = new_cache_provider(known_columns)
      {:ok, cache} = Cache.start_link(provider.cache_opts)

      Enum.each(
        known_columns,
        &assert(Cache.value(cache, provider.data_source, provider.table_name, &1) == {:isolated, &1})
      )
    end

    test "isolated error for unknown columns" do
      known_columns = ~w(col1 col2 col3)
      provider = new_cache_provider(known_columns)
      {:ok, cache} = Cache.start_link(provider.cache_opts)

      assert_raise(RuntimeError, fn ->
        Cache.value(cache, provider.data_source, provider.table_name, "unknown col")
      end)
    end

    test "prioritizing on demand" do
      known_columns = ~w(col1 col2 col3)

      {:ok, counter} = Agent.start_link(fn -> 0 end)

      provider =
        new_cache_provider(
          known_columns,
          property_fun:
            property_fun(%{
              "col1" => fn -> sleep_a_little_or_forever(counter) end,
              "col2" => fn -> sleep_a_little_or_forever(counter) end
            })
        )

      {:ok, cache} = Cache.start_link(provider.cache_opts)
      assert Cache.value(cache, provider.data_source, provider.table_name, "col3") == {:isolated, "col3"}
    end

    test "returns default for columns which failed to load" do
      known_columns = ~w(col1 col2 col3)

      ExUnit.CaptureLog.capture_log(fn ->
        provider =
          new_cache_provider(
            known_columns,
            property_fun: property_fun(%{"col1" => fn -> raise "error" end}),
            default: :a_default
          )

        {:ok, cache} = Cache.start_link(provider.cache_opts)
        assert :a_default = Cache.value(cache, provider.data_source, provider.table_name, "col1")
      end)
    end

    test "handling columns which failed to load while client is waiting for the result" do
      known_columns = ~w(col1 col2 col3)

      ExUnit.CaptureLog.capture_log(fn ->
        provider =
          new_cache_provider(
            known_columns,
            property_fun:
              property_fun(%{
                "col1" => fn -> Process.sleep(200) end,
                "col2" => fn -> raise "error" end
              }),
            default: :a_default
          )

        {:ok, cache} = Cache.start_link(provider.cache_opts)
        assert :a_default = Cache.value(cache, provider.data_source, provider.table_name, "col2")
      end)
    end

    test "handling data source changes" do
      known_columns = ~w(col1 col2 col3)

      provider = new_cache_provider(known_columns)
      {:ok, cache} = Cache.start_link(provider.cache_opts)

      Agent.update(provider.provider, fn columns ->
        Enum.map(columns, fn {datasource, table, name} -> {datasource, table, "updated #{name}"} end)
      end)

      send(cache, {:data_sources_changed, nil})

      assert Cache.value(cache, provider.data_source, provider.table_name, "updated col1") ==
               {:isolated, "updated col1"}

      assert_raise(RuntimeError, fn ->
        Cache.value(cache, provider.data_source, provider.table_name, "col1")
      end)
    end

    test "cache is persisted" do
      provider = new_cache_provider(~w(col1))
      {:ok, cache} = Cache.start_link(provider.cache_opts)

      # invoking `value` makes sure that isolated for `col1` is computed, and that the cache is persisted
      Cache.value(cache, provider.data_source, provider.table_name, "col1")
      GenServer.stop(cache)

      # make sure that new computation of isolated will never finish
      provider =
        put_in(
          provider.cache_opts[:property_fun],
          property_fun(%{"col1" => fn -> Process.sleep(:infinity) end})
        )

      {:ok, cache} = Cache.start_link(provider.cache_opts)
      assert Cache.value(cache, provider.data_source, provider.table_name, "col1") == {:isolated, "col1"}
    end

    test "cached items are not primed during cache start" do
      provider = new_cache_provider(~w(col1))
      {:ok, cache} = Cache.start_link(provider.cache_opts)

      # invoking `value` makes sure that isolated for `col1` is computed, and that the cache is persisted
      Cache.value(cache, provider.data_source, provider.table_name, "col1")
      GenServer.stop(cache)

      # add another column
      Agent.update(provider.provider, fn columns ->
        {datasource, table, _name} = hd(columns)
        new_column = {datasource, table, "col2"}
        [new_column | columns]
      end)

      me = self()

      provider =
        put_in(
          provider.cache_opts[:property_fun],
          property_fun(%{"col1" => fn -> send(me, {:computing, "col1"}) end})
        )

      {:ok, cache} = Cache.start_link(provider.cache_opts)
      assert Cache.value(cache, provider.data_source, provider.table_name, "col2") == {:isolated, "col2"}
      refute_receive {:computing, "col1"}
    end
  end

  describe ".update_with_remote_result" do
    test "columns updated from remote will not be recomputed" do
      known_columns = ~w(col1 col2 col3)

      {:ok, counter} = Agent.start_link(fn -> 0 end)

      compute = fn ->
        Agent.update(counter, fn i -> i + 1 end)
        Process.sleep(100)
      end

      provider =
        new_cache_provider(
          known_columns,
          property_fun:
            property_fun(%{
              "col1" => compute,
              "col2" => compute,
              "col3" => compute
            })
        )

      {:ok, cache} = Cache.start_link(provider.cache_opts)

      for col <- known_columns do
        Cache.update_with_remote_result(
          cache,
          Result.encrypt(
            Result.new(
              Descriptor.hash(provider.data_source, provider.table_name, col),
              Cloak.DataSource.Isolators.Cache,
              {:isolated, col},
              NaiveDateTime.utc_now()
            )
          )
        )
      end

      assert Cache.value(cache, provider.data_source, provider.table_name, "col1") == {:isolated, "col1"}
      assert Cache.value(cache, provider.data_source, provider.table_name, "col2") == {:isolated, "col2"}
      assert Cache.value(cache, provider.data_source, provider.table_name, "col2") == {:isolated, "col2"}
      assert Agent.get(counter, & &1) <= 1
    end
  end

  defp new_cache_provider(column_names, opts \\ []) do
    data_source = %{name: inspect(make_ref()), parameters: %{host: inspect(make_ref())}}
    table_name = inspect(make_ref())
    columns = Enum.map(column_names, &{data_source, table_name, &1})

    {:ok, provider} = Agent.start_link(fn -> columns end)

    cache_opts =
      Keyword.merge(
        [
          columns_provider: columns_provider(provider),
          property_fun: property_fun(),
          refresh_interval: :timer.hours(1),
          name: __MODULE__,
          cache_owner: __MODULE__,
          registered?: false,
          default: nil
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

  defp columns_provider(provider), do: fn _data_sources -> Agent.get(provider, & &1) end

  defp property_fun(map \\ %{}) do
    fn {_data_source_name, _table_name, column_name} ->
      with {:ok, fun} <- Map.fetch(map, column_name), do: fun.()
      {:isolated, column_name}
    end
  end

  defp sleep_a_little_or_forever(counter) do
    case Agent.get_and_update(counter, fn state -> {state, state + 1} end) do
      0 -> Process.sleep(200)
      1 -> Process.sleep(:infinity)
    end
  end
end
