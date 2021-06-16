defmodule SystemTest.Connectivity do
  use ExUnit.Case, async: true

  test "connectivity" do
    # Note: we're disabling proxies explicitly, because otherwise things break in MPI network
    response = HTTPoison.get!("http://air:8080/api/data_sources", %{"auth-token" => auth_token!()}, proxy: nil)
    assert response.status_code == 200

    data_sources = Jason.decode!(response.body)

    data_source_names = data_sources |> Enum.map(&Map.fetch!(&1, "name")) |> Enum.sort()
    assert data_source_names == ~w(postgresql)

    Enum.each(data_sources, fn data_source ->
      assert Map.fetch!(data_source, "errors") |> Enum.reject(&String.contains?(&1, " deprecated ")) == []

      tables = Map.fetch!(data_source, "tables")
      assert tables != []
      Enum.each(tables, &refute(Map.fetch!(&1, "broken")))
    end)
  end

  defp auth_token!() do
    ~w(#{Application.app_dir(:system_test)} priv dev admin_token)
    |> Path.join()
    |> File.read!()
    |> String.trim()
  end
end
