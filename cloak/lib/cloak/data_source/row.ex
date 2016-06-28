defmodule Cloak.DataSource.Row do
  @moduledoc """
  Represents a row of named fields.

  This module can be used to create and manage rows, where each row is an
  ordered collection of named fields. Once you create a row, you can use
  functions from this module to fetch values of desired fields.
  """

  defstruct [:columns, :values_map]

  @type t :: %__MODULE__{
    columns: [any],
    values_map: %{any => any}
  }

  @doc "Creates a row from specified names and values."
  @spec new([any], [any]) :: t
  def new(columns, fields) do
    %__MODULE__{
      columns: columns,
      values_map:
        Enum.zip(columns, fields) |> Enum.into(%{})
    }
  end

  @doc "Returns a value of the desired column."
  @spec value(t, any) :: any
  def value(row, column) do
    case Map.fetch(row.values_map, column) do
      {:ok, value} -> value
      :error ->
        raise(
          Cloak.Query.Runner.RuntimeError,
          "Column `#{column}` doesn't exist in selected columns."
        )
    end
  end

  @doc "Returns ordered values of all columns in the row."
  @spec values(t) :: [any]
  def values(row), do: values(row, row.columns)

  @doc "Returns ordered values of specified columns in the row."
  @spec values(t, [any]) :: [any]
  def values(row, columns) do
    Enum.map(columns, &value(row, &1))
  end
end
