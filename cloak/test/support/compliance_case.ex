defmodule ComplianceCase do
  @moduledoc false

  use ExUnit.CaseTemplate

  using(opts) do
    quote bind_quoted: [opts: opts] do
      @timeout Keyword.get(opts, :timeout, :timer.minutes(5))

      @moduletag :compliance
      @moduletag report: [:compliance]
      @moduletag timeout: @timeout + :timer.seconds(10)

      import ComplianceCase
    end
  end

  setup_all do
    for data_source <- data_sources(),
        {_, table} <- data_source.tables,
        column <- table.columns do
      Cloak.TestIsolatorsCache.forward_isolator(data_source, table.name, column.name)
      Cloak.TestShadowCache.forward(data_source, table.name, column.name)
      Cloak.TestBoundsCache.forward(data_source, table.name, column.name)
    end

    {:ok, data_sources: data_sources(), disabled: false}
  end

  import Cloak.Test.QueryHelpers

  @doc false
  def disable_for(context, driver, disable? \\ true)

  def disable_for(context, _driver, false), do: context
  def disable_for(context, :all, true), do: %{context | disabled: true}

  def disable_for(%{data_sources: data_sources} = context, driver, true),
    do: %{context | data_sources: Enum.reject(data_sources, &(&1.driver == driver))}

  @doc false
  defmacro assert_not_failing(context, query) do
    quote bind_quoted: [context: context, query: query] do
      cond do
        Enum.empty?(context.data_sources) ->
          :ok

        context.disabled ->
          :ok

        true ->
          delta = Map.get(context, :delta, 0.0001)
          assert_query_not_failing(query, delta: delta, data_sources: context.data_sources, timeout: @timeout)
      end
    end
  end

  @doc false
  defmacro assert_consistent_and_not_failing(context, query) do
    quote bind_quoted: [context: context, query: query] do
      cond do
        Enum.empty?(context.data_sources) ->
          :ok

        context.disabled ->
          :ok

        true ->
          delta = Map.get(context, :delta, 0.0001)
          result = assert_query_consistency(query, delta: delta, data_sources: context.data_sources, timeout: @timeout)

          if match?(%{error: _}, result) or match?({:exit, _}, result) do
            raise ExUnit.AssertionError,
              message: "Query execution failed. Query was:\n#{query}.\n\nError:\n#{inspect(result)}"
          else
            :ok
          end
      end
    end
  end

  @doc false
  def on_column(form, column_name), do: String.replace(form, "<col>", column_name)

  @doc false
  def on_columns(form, columns),
    do:
      columns
      |> Enum.with_index(1)
      |> Enum.reduce(form, fn {column, index}, acc ->
        String.replace(acc, "<col#{index}>", column)
      end)

  @doc false
  def all_columns() do
    Enum.concat([
      float_columns(),
      integer_columns(),
      numerical_columns(),
      datetime_columns(),
      date_columns(),
      text_columns(),
      nullable_columns()
    ])
    |> Enum.uniq()
  end

  @addresses_user_id_chain "addresses a inner join users u on u.id = a.user_fk"
  @notes_user_id_chain "notes n inner join users u on u.id = n.user_fk"
  @notes_changes_user_id_chain "notes_changes nc inner join notes n on nc.note_id = n.id inner join users u on u.id = n.user_fk"

  @doc false
  def float_columns(),
    do: [
      # {column name, table name, uid column in table}
      {"height", "users"},
      {"height", "users_public"},
      {"signed_float", "users"},
      {"signed_float", "users_public"}
    ]

  @doc false
  def integer_columns(),
    do: [
      # {column name, table name, uid column in table}
      {"age", "users"},
      {"age", "users_public"},
      {"signed_integer", "users"},
      {"signed_integer", "users_public"},
      {"n.id", @notes_user_id_chain},
      {"user_fk", @addresses_user_id_chain},
      {"work_postal_code", @addresses_user_id_chain},
      {"home_postal_code", @addresses_user_id_chain},
      {"user_fk", @notes_user_id_chain},
      {"note_id", @notes_changes_user_id_chain}
    ]

  @doc false
  def numerical_columns(), do: float_columns() ++ integer_columns()

  @doc false
  def raw_columns(columns), do: Enum.reject(columns, fn {name, _} -> String.contains?(name, "(") end)

  @doc false
  def datetime_columns(),
    do: [
      # {column name, table name, uid column in table}
      {"date", @notes_changes_user_id_chain}
    ]

  @doc false
  def date_columns(),
    do: [
      # {column name, table name, uid column in table}
      {"birthday", "users"},
      {"birthday", "users_public"}
    ]

  @doc false
  def text_columns(),
    do: [
      # {column name, table name, uid column in table}
      {"name_unicode", "users"},
      {"name_unicode", "users_public"},
      {"column_with_a_very_long_name", "users"},
      {"column_with_a_very_long_name", "users_public"},
      {"home_city", @addresses_user_id_chain},
      {"work_city", @addresses_user_id_chain},
      {"title", @notes_user_id_chain},
      {"content", @notes_user_id_chain},
      {"change", @notes_changes_user_id_chain}
    ]

  def nullable_columns(),
    do: [
      # {column name, table name, uid column in table}
      {"nullable", "users"},
      {"nullable", "users_public"}
    ]

  @doc false
  def table_uids(),
    do: [
      "users",
      @addresses_user_id_chain,
      @notes_user_id_chain,
      @notes_changes_user_id_chain
    ]

  @doc false
  def table_pairs(),
    do:
      for(
        table_uid1 <- table_uids(),
        table_uid2 <- table_uids(),
        do: {table_uid1, table_uid2}
      )

  @doc false
  def data_sources(), do: :global.trans({__MODULE__, self()}, &get_data_sources/0)

  defp get_data_sources() do
    # we're caching datasource definition to prevent repeated datasource reloading

    cached_data_sources = Application.get_env(:cloak, :cached_data_sources, %{})
    compliance_file = if System.get_env("CI") == "true", do: "dockerized_ci", else: "compliance"

    case Map.fetch(cached_data_sources, compliance_file) do
      :error ->
        data_sources = Compliance.DataSources.all_from_config_initialized(compliance_file)
        Application.put_env(:cloak, :cached_data_sources, Map.put(cached_data_sources, compliance_file, data_sources))
        for data_source <- data_sources, do: Cloak.DataSource.replace_data_source_config_sync(data_source)
        Cloak.TestIsolatorsCache.data_sources_changed()
        Cloak.TestShadowCache.data_sources_changed()
        Cloak.TestBoundsCache.data_sources_changed()
        data_sources

      {:ok, data_sources} ->
        data_sources
    end
    |> verify_data_sources()
  end

  defp verify_data_sources([_, _ | _] = data_sources), do: data_sources

  defp verify_data_sources(_),
    do: raise(ExUnit.AssertionError, message: "More than one data source is needed to ensure compliance")
end
