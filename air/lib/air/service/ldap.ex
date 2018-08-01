defmodule Air.Service.LDAP do
  require Aircloak.DeployConfig

  def simple_bind(config \\ Aircloak.DeployConfig.fetch("ldap"), user_dn, password) do
    with_connection(config, fn conn ->
      case :eldap.simple_bind(conn, user_dn, password) do
        :ok -> :ok
        {:error, {:gen_tcp_error, _}} -> {:error, :connect_failed}
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
    else
      {:error, 'connect failed'} -> {:error, :connect_failed}
      {:error, {:gen_tcp_error, _}} -> {:error, :connect_failed}
      other -> other
    end
  end

  defp open_connection(:error), do: {:error, :ldap_not_configured}

  defp open_connection({:ok, %{"host" => host, "port" => port, "encryption" => "start_tls"}}) do
    with {:ok, conn} <- :eldap.open([to_charlist(host)], port: port),
         options = [],
         timeout = :timer.seconds(5),
         :ok <- :eldap.start_tls(conn, options, timeout) do
      {:ok, conn}
    end
  end

  defp open_connection({:ok, %{"host" => host, "port" => port, "encryption" => "ssl"}}) do
    :eldap.open([to_charlist(host)], port: port, ssl: true)
  end

  defp open_connection({:ok, %{"host" => host, "port" => port}}) do
    :eldap.open([to_charlist(host)], port: port)
  end

  defp open_connection(_), do: {:error, :invalid_config}
end
