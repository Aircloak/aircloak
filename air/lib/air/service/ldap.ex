defmodule Air.Service.LDAP do
  @moduledoc "Wraps LDAP-based authentication."

  @timeout :timer.seconds(5)

  alias Air.Service.Settings
  alias Air.Schemas.User

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc """
  Returns true if the user's LDAP DN matches the password, false otherwise. Talks to the LDAP server as configured in
  `Air.Service.Settings`.
  """
  @spec validate_password(User.t(), String.t()) :: boolean
  def validate_password(ldap_dn, password) do
    if Settings.read().ldap_enabled do
      authenticate(ldap_dn, password)
    else
      false
    end
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp authenticate(ldap_dn, password) do
    with {:ok, connection} <- open_connection(),
         :ok <- Exldap.verify_credentials(connection, ldap_dn, password) do
      true
    else
      _ -> false
    end
  end

  defp open_connection() do
    Exldap.open(
      Settings.read().ldap_host,
      Settings.read().ldap_port,
      Settings.read().ldap_ssl,
      @timeout
    )
  end
end
