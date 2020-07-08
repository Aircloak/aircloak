defmodule Cloak.DataSource.Validations.Keys do
  @moduledoc """
  This is a helper module for validating data source keys.

  The following is validated:
  - the user id key can at most appear once per table
  """

  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @doc """
  Validates that the keys of a data source conforms to the rules.

  ## Examples

    iex> data_source = %{tables: %{table: %{keys: [%{user_id: "id"}]}}, errors: []}
    iex> Cloak.DataSource.Validations.Keys.ensure_permitted(data_source)
    %{tables: %{table: %{keys: [%{user_id: "id"}]}}, errors: []}

    iex> data_source = %{tables: %{table: %{keys: [%{user_id: "id1"}, %{user_id: "id2"}]}}, errors: []}
    iex> Cloak.DataSource.Validations.Keys.ensure_permitted(data_source)
    %{tables: %{table: %{keys: [%{user_id: "id1"}, %{user_id: "id2"}]}}, errors: [
      "Only one user-id key is allowed per table. Table `table` declares multiple: `id1` and `id2`"]}
  """
  @spec ensure_permitted(Map.t()) :: Map.t()
  def ensure_permitted(data_source), do: validate_uid_keys(data_source)

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp validate_uid_keys(%{tables: tables, errors: existing_errors} = data_source) do
    new_errors =
      tables
      |> Enum.map(fn {name, table} ->
        user_id_keys =
          Lens.key?(:keys)
          |> Lens.all()
          |> Lens.filter(&match?(%{user_id: _}, &1))
          |> Lens.to_list(table)
        {name, user_id_keys}
      end)
      |> Enum.filter(&(length(elem(&1, 1)) > 1))
      |> Enum.map(fn {table_name, user_ids} ->
        user_id_columns = user_ids |> Enum.map(&"`#{&1.user_id}`") |> Aircloak.OxfordComma.join()

        "Only one user-id key is allowed per table. Table `#{table_name}` declares multiple: #{user_id_columns}"
      end)

    %{data_source | errors: new_errors ++ existing_errors}
  end
end
