defmodule Compliance.Helpers do
  @moduledoc false

  import Cloak.Test.QueryHelpers

  @doc false
  def disable_for(context, _driver, false), do: context
  def disable_for(%{data_sources: data_sources} = context, driver, true), do:
    %{context | data_sources: Enum.reject(data_sources, & &1.driver == driver)}

  @doc false
  def assert_consistent_and_not_failing(%{data_sources: []}, query), do:
    raise ExUnit.AssertionError,
      message: "No data sources to execute query on. Query was:\n#{query}."
  def assert_consistent_and_not_failing(%{data_sources: data_sources}, query) do
    result = assert_query_consistency(query, data_sources: data_sources)
    if match?(%{error: _}, result) do
      raise ExUnit.AssertionError,
        message: "Query execution failed. Query was:\n#{query}.\n\nError:\n#{inspect result}"
    else
      :ok
    end
  end

  @doc false
  def on_column(form, column_name), do:
    String.replace(form, "<col>", column_name)

  @doc false
  def on_columns(form, columns), do:
    columns
    |> Enum.with_index(1)
    |> Enum.reduce(form, fn({column, index}, acc) -> String.replace(acc, "<col#{index}>", column) end)

  @doc false
  def float_columns(), do:
    [
      # {column name, table name, uid column in table}
      {"height", "users", "user_id"},
    ]

  @doc false
  def integer_columns(), do:
    [
      # {column name, table name, uid column in table}
      {"length(name)", "users", "user_id"},
      {"length(title)", "notes", "uid"},
      {"age", "users", "user_id"},
      {"id", "notes", "uid"},
      {"user_fk", "addresses", "uid"},
      {"home.postal_code", "addresses", "uid"},
      {"work.postal_code", "addresses", "uid"},
      {"length(home.city)", "addresses", "uid"},
      {"length(work.city)", "addresses", "uid"},
      {"user_fk", "notes", "uid"},
      {"length(content)", "notes", "uid"},
      {"length(title)", "notes_changes", "uid"},
      {"length(content)", "notes_changes", "uid"},
      {"length(changes.change)", "notes_changes", "uid"},
      {"note_id", "notes_changes", "uid"},
    ]

  @doc false
  def numerical_columns(), do: float_columns() ++ integer_columns()

  @doc false
  def datetime_columns(), do:
    [
      # {column name, table name, uid column in table}
      {"changes.date", "notes_changes", "uid"},
    ]

  @doc false
  def text_columns(), do:
    [
      # {column name, table name, uid column in table}
      {"name", "users", "user_id"},
      {"home.city", "addresses", "uid"},
      {"work.city", "addresses", "uid"},
      {"title", "notes", "uid"},
      {"content", "notes", "uid"},
      {"title", "notes_changes", "uid"},
      {"content", "notes_changes", "uid"},
      {"changes.change", "notes_changes", "uid"},
    ]
end
