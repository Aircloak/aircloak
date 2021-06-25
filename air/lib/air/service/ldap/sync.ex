defmodule Air.Service.LDAP.Sync do
  @moduledoc """
  This modules contains functions to synchronize a set of data obtained from LDAP (users and groups) with Air. It does
  not talk to LDAP itself, but merely takes the data it is supplied and applies synchronization rules, to reflect that
  data in Air.
  """

  require Logger
  import Ecto.Query
  alias Air.Service.LDAP.{User, Group}

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Synchronizes the given LDAP state (a list of users and groups) into Air."
  @spec sync([User.t()], [Group.t()]) :: :ok
  def sync(ldap_users, ldap_groups) do
    disable_missing_users(ldap_users)
    delete_missing_groups(ldap_groups)
    user_mappings = sync_users(ldap_users)
    sync_groups(ldap_groups, user_mappings)

    :ok
  end

  # -------------------------------------------------------------------
  # Group sync
  # -------------------------------------------------------------------

  defp sync_groups(ldap_groups, user_mappings), do: Enum.each(ldap_groups, &sync_group(&1, user_mappings))

  defp sync_group(ldap_group, user_mappings) do
    case Air.Repo.get_by(Air.Schemas.Group, ldap_dn: ldap_group.dn) do
      nil ->
        Logger.info("LDAP: Creating Aircloak Insights group corresponding to LDAP group #{ldap_group.name}.")
        create_group(ldap_group, user_mappings)
      air_group ->
        Logger.debug(fn -> "LDAP: Updating Aircloak Insights group corresponding to LDAP group #{ldap_group.name}." end)
        update_group(air_group, ldap_group, user_mappings)
    end
  end

  defp create_group(ldap_group, user_mappings) do
    ldap_group
    |> group_params(user_mappings)
    |> Air.Service.Group.create_ldap()
    |> check_group_error(ldap_group)
  end

  defp update_group(air_group, ldap_group, user_mappings) do
    air_group
    |> Air.Repo.preload(:users)
    |> Air.Service.Group.update(group_params(ldap_group, user_mappings), ldap: true)
    |> check_group_error(ldap_group)
  end

  defp group_params(ldap_group, user_mappings) do
    %{
      admin: false,
      name: ldap_group.name,
      ldap_dn: ldap_group.dn,
      users: Enum.map(ldap_group.member_ids, &Map.get(user_mappings, &1))
    }
  end

  defp delete_missing_groups(ldap_groups) do
    present_dns = Enum.map(ldap_groups, & &1.dn)

    missing_groups =
      Air.Schemas.Group
      |> where([q], q.source == ^:ldap)
      |> where([q], not (q.ldap_dn in ^present_dns))
      |> Air.Repo.all()

    unless Enum.empty?(missing_groups), do:
      Logger.info("LDAP: Deleting Aircloak Insights groups no longer returned by LDAP: #{joined_names(missing_groups)}")

    missing_groups
    |> Enum.each(&Air.Service.Group.delete!(&1, ldap: true))
  end

  defp check_group_error({:ok, result}, _), do: {:ok, result}

  defp check_group_error({:error, _}, ldap_group) do
    Logger.error(
      "LDAP: Failed to sync group `#{ldap_group.dn}` from LDAP." <>
        " The most likely cause is a name conflict with another LDAP group."
    )

    :error
  end

  # -------------------------------------------------------------------
  # Group sync
  # -------------------------------------------------------------------

  defp sync_users(ldap_users) do
    for ldap_user <- ldap_users do
      case sync_user(ldap_user) do
        {:ok, air_user} -> {ldap_user.login, air_user.id}
        _ -> nil
      end
    end
    |> Enum.filter(& &1)
    |> Enum.into(%{})
  end

  defp sync_user(ldap_user) do
    if air_user = Air.Repo.get_by(Air.Schemas.User, ldap_dn: ldap_user.dn) |> Air.Repo.preload(:logins) do
      Logger.debug(fn -> "LDAP: Updating Aircloak Insights user account for LDAP user #{ldap_user.login}." end)
      update_user(air_user, ldap_user)
    else
      Logger.info("LDAP: Creating Aircloak Insights  user account for LDAP user #{ldap_user.login}.")
      create_user(ldap_user)
    end
  end

  defp create_user(ldap_user) do
    Air.Service.User.create_ldap(%{
      login: ldap_user.login,
      ldap_dn: ldap_user.dn,
      name: ldap_user.name
    })
    |> check_user_error(ldap_user)
  end

  defp update_user(air_user, ldap_user) do
    Air.Service.User.update(air_user, %{login: ldap_user.login, name: ldap_user.name}, ldap: true)
    |> check_user_error(ldap_user)
    |> case do
      {:ok, air_user} ->
        Air.Service.User.enable!(air_user, ldap: true)
        {:ok, air_user}

      :error ->
        Air.Service.User.disable(air_user, ldap: true)
        :error
    end
  end

  defp disable_missing_users(ldap_users) do
    present_dns = Enum.map(ldap_users, & &1.dn)

    missing_users =
      Air.Schemas.User
      |> where([q], q.source == ^:ldap)
      |> where([q], not (q.ldap_dn in ^present_dns))
      |> Air.Repo.all()

    unless Enum.empty?(missing_users), do:
      Logger.info("LDAP: Aircloak Insights and LDAP hob no longer returned by LDAP: #{joined_names(missing_users)}")

    missing_users
    |> Enum.each(&Air.Service.User.disable(&1, ldap: true))
  end

  defp check_user_error({:ok, result}, _), do: {:ok, result}

  defp check_user_error({:error, _}, ldap_user) do
    Logger.error(
      "LDAP: Failed to sync user `#{ldap_user.dn}` from LDAP." <>
        " The most likely cause is a login conflict with another user."
    )

    :error
  end

  defp joined_names(entities), do:
    entities
    |> Enum.map(fn entity ->
      case entity do
        %Air.Schemas.User{} = user -> Air.Service.User.main_login(user)
        %Air.Schemas.Group{} = group -> group.name
      end
    end)
    |> Aircloak.OxfordComma.join()
end
