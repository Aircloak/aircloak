defmodule Air.Service.LDAP do
  require Aircloak.DeployConfig

  alias __MODULE__.User

  @timeout :timer.seconds(5)

  def simple_bind(config \\ Aircloak.DeployConfig.fetch("ldap"), user_dn, password) do
    with_connection(config, fn conn, _config ->
      case :eldap.simple_bind(conn, user_dn, password) do
        :ok -> :ok
        {:error, {:gen_tcp_error, _}} -> {:error, :connect_failed}
        _ -> {:error, :invalid_credentials}
      end
    end)
  end

  def users(config \\ Aircloak.DeployConfig.fetch("ldap")) do
    with_bound_connection(config, fn conn, config ->
      :eldap.search(conn, base: config["user_base"], filter: :eldap.present('objectClass'))
      |> case do
        {:ok, {:eldap_search_result, results, _}} -> {:ok, Enum.map(results, &build_user(config, &1))}
        _ -> {:error, :search_failed}
      end
    end)
  end

  defp build_user(_config, {:eldap_entry, dn, _fields}), do: %User{dn: to_string(dn), name: to_string(dn)}

  defp with_bound_connection(config, action) do
    with_connection(config, fn conn, config ->
      case :eldap.simple_bind(conn, Map.get(config, "bind_dn", ""), Map.get(config, "password", "")) do
        :ok -> action.(conn, config)
        _ -> {:error, :connect_failed}
      end
    end)
  end

  defp with_connection(config, action) do
    with {:ok, conn} <- open_connection(config), {:ok, config} <- config do
      try do
        action.(conn, config)
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
         :ok <- :eldap.start_tls(conn, _options = [], @timeout) do
      {:ok, conn}
    end
  end

  defp open_connection({:ok, %{"host" => host, "port" => port, "encryption" => "ssl"}}) do
    :eldap.open([to_charlist(host)], port: port, ssl: true, timeout: @timeout)
  end

  defp open_connection({:ok, %{"host" => host, "port" => port}}) do
    :eldap.open([to_charlist(host)], port: port, timeout: @timeout)
  end

  defp open_connection(_), do: {:error, :invalid_config}
end
