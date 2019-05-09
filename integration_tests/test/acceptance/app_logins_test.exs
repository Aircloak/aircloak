defmodule IntegrationTest.Acceptance.AppLoginsTest do
  use IntegrationTest.AcceptanceCase, async: true
  import IntegrationTest.Manager

  test "creating an app login" do
    {login, password} = create_app_login()
    assert {:ok, _conn} = connect(login, password)
  end

  test "revoking an app login" do
    Process.flag(:trap_exit, true)
    {login, password} = create_app_login()
    click({:xpath, "//td[text()='#{login}']/..//a[text()='Revoke']"})
    accept_dialog()
    assert_has(:xpath, "//*[contains(text(), 'Login revoked')]")
    assert {:error, _reason} = connect(login, password)
  end

  defp create_app_login() do
    login_as_admin()
    click({:css, "#navbar_dropdown"})
    click({:xpath, "//header/nav//a[text()='App logins']"})

    fill_field({:css, "#login_description"}, random_string())
    click({:xpath, "//button[text()='Create login']"})
    assert_has(:xpath, "//*[contains(text(), 'Your new login has been added')]")

    %{"login" => login, "password" => password} =
      visible_text({:xpath, "//*[contains(text(), 'login:') and contains(text(), 'password:')]"})
      |> String.trim()
      |> String.split("\n")
      |> Enum.map(&String.split(&1, ": "))
      |> Enum.map(&List.to_tuple/1)
      |> Map.new()

    {login, password}
  end

  defp connect(login, password) do
    Postgrex.start_link(
      hostname: "localhost",
      port: Application.fetch_env!(:air, Air.PsqlServer) |> Keyword.fetch!(:port),
      username: login,
      password: password,
      database: data_source_name(),
      ssl: true,
      sync_connect: true,
      backoff_type: :stop
    )
  end
end
