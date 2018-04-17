defmodule IntegrationTest.PrivacyPolicyTest do
  # `async: false` because we need to do some aggresive user deletion actions
  # to get around foreign key constraints on the user ---> privacy policy relation.
  # We do not want other tests creating users while this is going on.
  # (see https://hexdocs.pm/ecto/Ecto.Adapters.SQL.Sandbox.html)
  use ExUnit.Case, async: false

  alias IntegrationTest.Manager

  setup do
    user = Manager.create_air_user()
    {:ok, conn} = connect(user)
    {:ok, user: user, conn: conn}
  end

  test "error when no privacy policy exists", %{conn: conn} do
    # We need to delete all the privacy policies, but have to remove any previous
    # records of users having accepted the privacy policies, otherwise this fails.
    Air.Repo.update_all(Air.Schemas.User, set: [accepted_privacy_policy_id: nil])
    Air.Repo.delete_all(Air.Schemas.PrivacyPolicy)

    assert {:error, error} = Postgrex.query(conn, "select * from users", [])
    assert error.postgres.message =~ "No privacy policy has been set up"

    # Recreate the privacy policy to not trip up the other tests
    Air.Service.PrivacyPolicy.set("Privacy policy content")
  end

  test "error when privacy policy has not been accepted", %{conn: conn, user: user} do
    Air.Service.User.reject_privacy_policy!(user)
    assert {:error, error} = Postgrex.query(conn, "select * from users", [])
    assert error.postgres.message =~ "review and accept the privacy policy"
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
