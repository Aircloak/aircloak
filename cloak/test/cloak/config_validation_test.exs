defmodule Cloak.ConfigValidationTest do
  use ExUnit.Case, async: true

  describe "config.json" do
    test "error on missing mandatory fields" do
      Enum.each(~w(air_site salt data_sources), &assert_missing_field_reported(&1, config_validator()))
    end

    test "optional fields are not required" do
      Enum.each(~w(debug features air_socket_url), &refute_missing_field_reported(&1, config_validator()))
    end

    test "error on invalid fields" do
      assert_invalid_field_reported(nil, config_validator())
    end
  end

  describe "data source" do
    test "error on missing mandatory fields" do
      Enum.each(~w(name driver parameters tables), &assert_missing_field_reported(&1, datasource_validator()))
      assert_missing_field_reported("parameters/hostname", datasource_validator())
      assert_missing_field_reported("tables/foo/db_name", datasource_validator())

      Enum.each(
        ~w(table foreign_key primary_key),
        &assert_missing_field_reported("tables/foo/projection/#{&1}", datasource_validator())
      )

      Enum.each(
        ~w(method columns),
        &assert_missing_field_reported("tables/foo/decoders/[]/#{&1}", datasource_validator())
      )
    end

    test "optional fields are not required" do
      refute_missing_field_reported("marker", datasource_validator())

      Enum.each(
        ~w(port username database password),
        &refute_missing_field_reported("parameters/#{&1}", datasource_validator())
      )

      Enum.each(
        ~w(user_id ignore_unsupported_types sample_rate projection decoders),
        &refute_missing_field_reported("tables/foo/#{&1}", datasource_validator())
      )

      refute_missing_field_reported("tables/foo/projection/user_id_alias", datasource_validator())
      refute_missing_field_reported("tables/foo/decoders/[]/key", datasource_validator())
    end

    test "invalid fields" do
      assert_invalid_field_reported(nil, datasource_validator())
      assert_invalid_field_reported("tables/foo", datasource_validator())
      assert_invalid_field_reported("tables/foo/projection", datasource_validator())

      refute_invalid_field_reported("parameters", datasource_validator())
      refute_invalid_field_reported("tables/foo/decoders/[]", datasource_validator())
    end
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp assert_missing_field_reported(path, validator) do
    {error, missing} = validate_with_missing_child(path, validator)
    assert error.message =~ ~r/Required property #{missing} was not present/
  end

  defp refute_missing_field_reported(path, validator) do
    {error, missing} = validate_with_missing_child(path, validator)
    refute error.message =~ ~r/Required property #{missing} was not present/
  end

  defp assert_invalid_field_reported(path, validator) do
    path = if(is_nil(path), do: [], else: String.split(path, "/")) ++ ["invalid_field"]
    json = generate_object(path)
    error = assert_raise(RuntimeError, fn -> validator.(json) end)

    assert error.message =~ ~r[#/#{Enum.join(path, "/")}: Schema does not allow additional properties.]
  end

  defp refute_invalid_field_reported(path, validator) do
    path = if(is_nil(path), do: [], else: String.split(path, "/")) ++ ["invalid_field"]
    json = generate_object(path)
    error = assert_raise(RuntimeError, fn -> validator.(json) end)

    refute error.message =~ ~r[invalid_field: Schema does not allow additional properties.]
  end

  defp validate_with_missing_child(path, validator) do
    {parents, missing} =
      path
      |> String.split("/")
      |> Enum.split(-1)

    json = generate_object(parents)
    error = assert_raise(RuntimeError, fn -> validator.(json) end)
    {error, missing}
  end

  defp generate_object(parents) do
    parents
    |> Enum.reverse()
    |> Enum.reduce(%{}, fn
      "[]", acc -> [acc]
      el, acc -> %{el => acc}
    end)
  end

  defp config_validator(), do: fn data -> Aircloak.DeployConfig.validate!(:cloak, data) end

  defp datasource_validator() do
    fn data ->
      case Aircloak.validate_decoded_json(:cloak, "datasource_schema.json", data, "validation error") do
        :ok -> :ok
        {:error, error} -> raise(error)
      end
    end
  end
end
