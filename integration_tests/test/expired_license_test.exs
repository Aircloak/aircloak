defmodule IntegrationTest.ExpiredLicenseTest do
  use ExUnit.Case, async: false

  alias IntegrationTest.Manager

  test "querying when license is invalid fails" do
    Manager.load_expired_license()
    on_exit(fn -> Manager.load_valid_license() end)
    {:ok, conn} = connect(Manager.create_air_user())

    assert {:error, %{postgres: %{message: message}}} = Postgrex.query(conn, "select * from users", [])

    assert message =~ "The license for this Aircloak instance has expired."
  end

  defp connect(user) do
    Postgrex.start_link(
      hostname: "localhost",
      port: Application.fetch_env!(:air, Air.PsqlServer) |> Keyword.fetch!(:port),
      username: user.email,
      password: Manager.user_password(),
      database: Manager.data_source_name(),
      ssl: true
    )
  end
end
