defmodule Air.Service.LDAP do
  @moduledoc "This module provides entry-point functions for interacting with LDAP."

  require Aircloak.DeployConfig

  alias __MODULE__.{User, Group, FilterParser}

  @timeout :timer.seconds(5)

  @type ldap_error ::
          :connect_failed | :invalid_credentials | :ldap_not_configured | :user_filter_invalid | :group_filter_invalid

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Attempts to authenticate the user with the given LDAP DN with the given password."
  @spec simple_bind({:ok, map()} | :error, String.t(), String.t()) :: :ok | {:error, ldap_error}
  def simple_bind(config \\ Aircloak.DeployConfig.fetch("ldap"), user_dn, password) do
    with_connection(config, fn conn, _config ->
      case :eldap.simple_bind(conn, user_dn, password) do
        :ok -> :ok
        {:error, {:gen_tcp_error, _}} -> {:error, :connect_failed}
        _ -> {:error, :invalid_credentials}
      end
    end)
  end

  @doc "Fetches the list of users from LDAP according to the provided configuration."
  @spec users({:ok, map()} | :error) :: {:ok, [User.t()]} | {:error, ldap_error}
  def users(config \\ Aircloak.DeployConfig.fetch("ldap")) do
    with_bound_connection(config, fn conn, config ->
      with {:ok, filter} <- parse_filter(config["user_filter"]),
           {:ok, results} <- search(conn, base: config["user_base"], filter: filter) do
        {:ok, results |> Enum.map(&build_user(config, &1)) |> Enum.reject(&is_nil(&1.login))}
      else
        {:error, :filter_invalid} -> {:error, :user_filter_invalid}
        other -> other
      end
    end)
  end

  @doc "Fetches the list of groups from LDAP according to the provided configuration."
  @spec groups({:ok, map()} | :error) :: {:ok, [Group.t()]} | {:error, ldap_error}
  def groups(config \\ Aircloak.DeployConfig.fetch("ldap")) do
    with_bound_connection(config, fn conn, config ->
      with {:ok, filter} <- parse_filter(config["group_filter"]),
           {:ok, results} <- search(conn, base: config["group_base"], filter: filter) do
        {:ok, results |> Enum.map(&build_group(config, &1)) |> Enum.reject(&is_nil(&1.name))}
      else
        {:error, :filter_invalid} -> {:error, :group_filter_invalid}
        other -> other
      end
    end)
  end

  # -------------------------------------------------------------------
  # Helpers for parsing LDAP data
  # -------------------------------------------------------------------

  defp parse_filter(nil), do: {:ok, :eldap.present('objectClass')}

  defp parse_filter(filter) do
    case FilterParser.parse(filter) do
      {:ok, filter} -> {:ok, filter}
      :error -> {:error, :filter_invalid}
    end
  end

  defp search(conn, options) do
    case :eldap.search(conn, options) do
      {:ok, {:eldap_search_result, results, _}} -> {:ok, results}
      _ -> {:error, :search_failed}
    end
  end

  defp build_group(config, {:eldap_entry, dn, fields}) do
    %Group{
      dn: to_string(dn),
      name: group_name(config, fields, dn),
      member_ids: attributes(fields, Map.get(config, "group_member", "memberUid"))
    }
  end

  defp group_name(config, fields, dn) do
    case Map.fetch(config, "group_name") do
      {:ok, name} -> attribute(fields, name)
      :error -> to_string(dn)
    end
  end

  defp build_user(config, {:eldap_entry, dn, fields}) do
    %User{
      dn: to_string(dn),
      name: attribute(fields, Map.get(config, "user_name", "cn")),
      login: attribute(fields, Map.get(config, "user_login", "cn"))
    }
  end

  defp attribute(fields, name, default \\ nil) do
    case attributes(fields, name) do
      [] -> default
      [value | _rest] -> value
    end
  end

  defp attributes(fields, name) do
    name = to_charlist(name)

    fields
    |> Enum.find(fn {field_name, _} -> field_name == name end)
    |> case do
      {_, values} -> Enum.map(values, &to_string/1)
      _ -> []
    end
  end

  # -------------------------------------------------------------------
  # Connection helpers
  # -------------------------------------------------------------------

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
