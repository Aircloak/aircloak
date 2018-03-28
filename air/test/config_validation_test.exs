defmodule Air.ConfigValidationTest do
  use ExUnit.Case, async: true

  test "error on missing mandatory fields" do
    assert_missing_field_reported("database")
    Enum.each(~w(host user name), &assert_missing_field_reported("database/#{&1}"))

    assert_missing_field_reported("site")

    Enum.each(
      ~w(auth_secret endpoint_key_base api_token_salt master_password),
      &assert_missing_field_reported("site/#{&1}")
    )

    Enum.each(~w(require_ssl), &assert_missing_field_reported("psql_server/#{&1}"))
  end

  test "optional fields are not required" do
    Enum.each(~w(port ssl password), &refute_missing_field_reported("database/#{&1}"))

    Enum.each(
      ~w(certfile keyfile use_staging_license_key),
      &refute_missing_field_reported("site/#{&1}")
    )

    refute_missing_field_reported("psql_server")
    Enum.each(~w(certfile keyfile), &refute_missing_field_reported("psql_server/#{&1}"))
  end

  test "error on invalid fields" do
    assert_invalid_field_reported()
    assert_invalid_field_reported("site")
    assert_invalid_field_reported("psql_server")
    assert_invalid_field_reported("database")
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp assert_missing_field_reported(path) do
    {error, missing} = validate_with_missing_child(path)
    assert error.message =~ ~r/Required property #{missing} was not present/
  end

  defp refute_missing_field_reported(path) do
    {error, missing} = validate_with_missing_child(path)
    refute error.message =~ ~r/Required property #{missing} was not present/
  end

  defp assert_invalid_field_reported(path \\ nil) do
    path = if(is_nil(path), do: [], else: String.split(path, "/")) ++ ["invalid_field"]
    json = generate_object(path)
    error = assert_raise(RuntimeError, fn -> Aircloak.DeployConfig.validate!(:air, json) end)

    assert error.message =~ ~r[#/#{Enum.join(path, "/")}: Schema does not allow additional properties.]
  end

  defp validate_with_missing_child(path) do
    {parents, missing} =
      path
      |> String.split("/")
      |> Enum.split(-1)

    json = generate_object(parents)
    error = assert_raise(RuntimeError, fn -> Aircloak.DeployConfig.validate!(:air, json) end)
    {error, missing}
  end

  defp generate_object(parents) do
    parents
    |> Enum.reverse()
    |> Enum.reduce(%{}, fn el, acc -> %{el => acc} end)
  end
end
