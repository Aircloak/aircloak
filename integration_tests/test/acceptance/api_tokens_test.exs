defmodule IntegrationTest.Acceptance.ApiTokensTest do
  use IntegrationTest.AcceptanceCase, async: true

  test "creating a token and using it" do
    token = create_token().token
    assert {:ok, response} = api_get(token, "/api/data_sources")
    assert response.status_code == 200
    assert response.body =~ "integers"
  end

  test "revoking a token" do
    %{token: token, description: description} = create_token()
    click({:xpath, "//td[text()='#{description}']/..//a[text()='Revoke']"})
    accept_dialog()
    assert_has(:xpath, "//*[contains(text(), 'Token revoked')]")
    assert {:ok, response} = api_get(token, "/api/data_sources")
    assert response.status_code == 401
    assert response.body =~ "Invalid auth-token"
  end

  defp api_get(token, path) do
    port = Application.fetch_env!(:air, AirWeb.Endpoint) |> Keyword.fetch!(:http) |> Keyword.fetch!(:port)
    HTTPoison.get("http://localhost:#{port}#{path}", %{"auth-token" => token})
  end

  defp create_token() do
    login_as_admin()
    click({:css, "#navbar_dropdown"})
    click({:xpath, "//header/nav//a[text()='API Tokens']"})

    description = random_string()
    fill_field({:css, "#api_token_description"}, description)
    click({:xpath, "//button[text()='Create token']"})

    assert_has(:xpath, "//*[contains(text(), 'Your new token has been added')]")
    token = visible_text({:xpath, "//*[contains(text(), 'Your new token has been added')]/..//pre"})

    %{token: token, description: description}
  end
end
