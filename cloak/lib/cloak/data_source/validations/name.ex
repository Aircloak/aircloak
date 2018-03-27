defmodule Cloak.DataSource.Validations.Name do
  @moduledoc """
  This is a helper module for validating data source names.

  The decision has been made that names should follow the naming rules
  of Postgres database names. More specifically this entails that names should

  - be 31 characters or less
  - consist only of alphanumeric characters and underscores
  - start with an underscore or a letter
  - not be a restricted SQL keyword
  """

  alias Cloak.DataSource.Parameters

  @max_name_length 31

  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @doc """
  Validates that a data source name conforms to the naming rules.
  Adds errors to the errors-field in the data source if naming rules are broken.
  """
  @spec ensure_permitted(Map.t()) :: Map.t()
  def ensure_permitted(data_source) do
    data_source
    |> validate_not_too_long()
    |> validate_used_characters()
    |> validate_first_character()
    |> validate_no_keyword()
    |> validate_has_name()
  end

  def check_for_duplicates(data_sources) do
    duplicate_names =
      data_sources
      |> Enum.group_by(& &1.name)
      |> Enum.filter(fn {_name, values} -> Enum.count(values) > 1 end)
      |> Enum.map(fn {name, _values} -> name end)

    Enum.map(data_sources, fn %{name: name, errors: errors} = data_source ->
      if name in duplicate_names do
        error =
          "The cloak has been configured with duplicate entries for the data source " <>
            "named #{name}. The data source will not behave as expected."

        %{data_source | errors: [error | errors]}
      else
        data_source
      end
    end)
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp validate_not_too_long(data_source),
    do:
      add_error_if(
        data_source,
        &(String.length(&1) > @max_name_length),
        "The data source name is too long. It cannot exceed 31 charactes in length"
      )

  defp validate_used_characters(data_source),
    do:
      add_error_if(
        data_source,
        &(&1 =~ ~r/[^A-z_0-9]/),
        "The data source name is not valid. It can only consist of alphanumeric characters and underscores"
      )

  defp validate_first_character(data_source),
    do:
      add_error_if(
        data_source,
        &(&1 =~ ~r/^[^A-z_]+/),
        "The data source name is not valid. It must start with a character or an underscore"
      )

  defp validate_no_keyword(data_source),
    do:
      add_error_if(
        data_source,
        &Enum.member?(Cloak.Sql.Lexer.keywords(), String.upcase(&1)),
        "The data source name cannot be a reserved SQL keyword like SELECT, FROM, etc"
      )

  defp validate_has_name(%{errors: errors} = data_source) do
    if Map.get(data_source, :name, "") === "" do
      database = Parameters.get_one_of(data_source.parameters, ["database"])
      host = Parameters.get_one_of(data_source.parameters, ["hostname", "server", "host"])

      error =
        "The data source for database #{database} on host #{host} needs to be configured with a name"

      Map.put(data_source, :errors, [error | errors])
    else
      data_source
    end
  end

  defp add_error_if(%{errors: errors} = data_source, validator, error_message) do
    name = Map.get(data_source, :name, "")

    if validator.(name) do
      %{data_source | errors: [error_message | errors]}
    else
      data_source
    end
  end
end
