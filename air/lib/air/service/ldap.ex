defmodule Air.Service.LDAP do
  require Aircloak.DeployConfig

  def simple_bind(config \\ Aircloak.DeployConfig.fetch("ldap"), user_dn, password) do
    with_connection(config, fn conn ->
      case :eldap.simple_bind(conn, user_dn, password) do
        :ok -> :ok
        _ -> {:error, :invalid_credentials}
      end
    end)
  end

  defp with_connection(config, action) do
    with {:ok, conn} <- open_connection(config) do
      try do
        action.(conn)
      after
        :eldap.close(conn)
      end
    end
  end

  defp open_connection(:error), do: {:error, :ldap_not_configured}

  defp open_connection({:ok, %{"host" => host, "port" => port}}) do
    :eldap.open([to_charlist(host)], port: port)
  end
end
