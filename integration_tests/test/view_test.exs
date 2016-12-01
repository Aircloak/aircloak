defmodule IntegrationTest.ViewTest do
  use ExUnit.Case, async: true

  alias IntegrationTest.Manager

  test "reporting view error" do
    assert {:error, error} = validate_view("select")
    assert error =~ ~r/Expected `column definition`/
  end

  test "successful view validation", do:
    assert :ok == validate_view("select user_id, name from users")

  defp validate_view(sql), do:
    Air.Service.DataSource.validate_view(
      {:global_id, Manager.data_source_global_id()},
      Manager.air_user(),
      sql
    )
end
