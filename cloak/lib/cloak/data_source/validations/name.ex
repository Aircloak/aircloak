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

  @max_name_length 31

  #-----------------------------------------------------------------------------------------------------------
  # API
  #-----------------------------------------------------------------------------------------------------------

  def ensure_permitted(data_source) do
    data_source
    |> validate_length()
    |> validate_used_characters()
    |> validate_first_character()
    |> validate_no_keyword()
  end

  #-----------------------------------------------------------------------------------------------------------
  # Internal functions
  #-----------------------------------------------------------------------------------------------------------

  defp validate_length(data_source), do:
    add_error_if(data_source, & String.length(&1) > @max_name_length,
      "The data source name is too long. It cannot exceed 31 charactes in length")

  defp validate_used_characters(data_source), do:
    add_error_if(data_source, & &1 =~ ~r/[^A-z_0-9]/,
      "The data source name is not valid. It can only consist of alphanumeric characters and underscores")

  defp validate_first_character(data_source), do:
    add_error_if(data_source, & &1 =~ ~r/^[^A-z_]+/,
      "The data source name is not valid. It must start with a character or an underscore")

  defp validate_no_keyword(data_source), do:
    add_error_if(data_source, & Enum.member?(Cloak.Sql.Lexer.keywords(), String.upcase(&1)),
      "The data source name cannot be a reserved SQL keyword like SELECT, FROM, etc")

  defp add_error_if(%{name: name, errors: errors} = data_source, validator, error_message) do
    if validator.(name) do
      %{data_source | errors: [error_message | errors]}
    else
      data_source
    end
  end
end
