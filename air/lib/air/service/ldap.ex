defmodule Air.Service.LDAP do
  require Aircloak.DeployConfig

  alias __MODULE__.{User, Group}

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
      with {:ok, results} <- search(conn, base: config["user_base"], filter: :eldap.present('objectClass')) do
        {:ok, results |> Enum.map(&build_user(config, &1)) |> Enum.reject(&is_nil(&1.login))}
      end
    end)
  end

  def groups(config \\ Aircloak.DeployConfig.fetch("ldap")) do
    with_bound_connection(config, fn conn, config ->
      with {:ok, results} <- search(conn, base: config["group_base"], filter: :eldap.present('objectClass')) do
        {:ok, results |> Enum.map(&build_group(config, &1)) |> Enum.reject(&is_nil(&1.name))}
      end
    end)
  end

  defp search(conn, options) do
    case :eldap.search(conn, options) do
      {:ok, {:eldap_search_result, results, _}} -> {:ok, results}
      _ -> {:error, :search_failed}
    end
  end

  defp build_group(config, {:eldap_entry, dn, fields}) do
    %Group{dn: to_string(dn), name: attribute(fields, Map.get(config, "group_name", "cn")), member_dns: []}
  end

  defp build_user(config, {:eldap_entry, dn, fields}) do
    %User{
      dn: to_string(dn),
      name: attribute(fields, Map.get(config, "user_name", "cn")),
      login: attribute(fields, Map.get(config, "user_login", "cn"))
    }
  end

  defp attribute(fields, name, default \\ nil) do
    name = to_charlist(name)

    fields
    |> Enum.find(fn {field_name, _} -> field_name == name end)
    |> case do
      {_, [value | _rest]} -> to_string(value)
      _ -> default
    end
  end

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
