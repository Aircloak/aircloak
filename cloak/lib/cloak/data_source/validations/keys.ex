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
  def ensure_permitted(data_source),
    do:
      data_source
      |> validate_uid_keys()

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp validate_uid_keys(%{tables: tables, errors: existing_errors} = data_source) do
    new_errors =
      tables
      |> Enum.map(fn {table_name, data} ->
        {
          table_name,
          Lens.key?(:keys)
          |> Lens.all()
          |> Lens.filter(&match?(%{user_id: _}, &1))
          |> Lens.to_list(data)
        }
      end)
      |> Enum.filter(&(length(elem(&1, 1)) > 1))
      |> Enum.map(fn {table, keys} ->
        wrapped_keys =
          keys
          |> Enum.map(&Map.get(&1, :user_id))
          |> Enum.map(&"`#{&1}`")
          |> Aircloak.OxfordComma.join()

        "Only one user-id key is allowed per table. Table `#{table}` declares multiple: #{wrapped_keys}"
      end)

    %{data_source | errors: new_errors ++ existing_errors}
  end
end
