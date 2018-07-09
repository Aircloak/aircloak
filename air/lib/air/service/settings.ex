defmodule Air.Service.Settings do
  @moduledoc "Services for reading and writing Air-wide options."

  use GenServer

  import Ecto.Changeset

  @required_fields ~w(audit_log_enabled decimal_sep decimal_digits)a
  @optional_fields ~w(query_retention_days thousand_sep)a
  @ldap_fields ~w(ldap_enabled ldap_host ldap_port ldap_ssl ldap_ca_cert)a

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Returns the current version of the settings."
  @spec read() :: Air.Settings.t()
  def read(server \\ __MODULE__), do: GenServer.call(server, :read)

  @doc "Saves the specified settings."
  @spec save(%{optional(atom) => any()}) :: {:ok, Air.Schemas.Settings.t()} | {:error, Ecto.Changeset.t()}
  def save(server \\ __MODULE__, params), do: GenServer.call(server, {:save, params})

  @doc "Saves the specified LDAP settings."
  @spec save_ldap(Map.t()) :: {:ok, Air.Schemas.Settings.t()} | {:error, Ecto.Changeset.t()}
  def save_ldap(server \\ __MODULE__, params), do: GenServer.call(server, {:save_ldap, params})

  @doc "Returns the changeset for the latest settings."
  @spec latest_changeset() :: Ecto.Changeset.t()
  def latest_changeset(server \\ __MODULE__), do: GenServer.call(server, :latest_changeset)

  # -------------------------------------------------------------------
  # GenServer callbacks
  # -------------------------------------------------------------------

  def start_link(_args), do: GenServer.start_link(__MODULE__, nil, name: __MODULE__)

  @impl GenServer
  def init(_args), do: {:ok, latest_settings()}

  @impl GenServer
  def handle_call(:read, _from, state), do: {:reply, parse(state), state}

  def handle_call({:save, params}, _from, state) do
    case state |> changeset(params) |> do_save() do
      {:ok, updated} -> {:reply, {:ok, updated}, updated}
      {:error, changeset} -> {:reply, {:error, changeset}, state}
    end
  end

  def handle_call({:save_ldap, params}, _from, state) do
    case state |> ldap_changeset(params) |> do_save() do
      {:ok, updated} -> {:reply, {:ok, updated}, updated}
      {:error, changeset} -> {:reply, {:error, changeset}, state}
    end
  end

  def handle_call(:latest_changeset, _from, state), do: {:reply, changeset(state), state}

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp do_save(changeset) do
    if changeset.data.id,
      do: Air.Repo.update(changeset),
      else: Air.Repo.insert(changeset)
  end

  defp parse(schema) do
    %Air.Settings{
      query_retention_days: unserialize_retention_days(schema.query_retention_days),
      audit_log_enabled: schema.audit_log_enabled,
      decimal_sep: schema.decimal_sep,
      thousand_sep: schema.thousand_sep || "",
      decimal_digits: schema.decimal_digits,
      ldap_enabled: schema.ldap_enabled,
      ldap_host: schema.ldap_host,
      ldap_port: schema.ldap_port,
      ldap_ssl: schema.ldap_ssl,
      ldap_ca_cert: schema.ldap_ca_cert
    }
  end

  defp latest_settings() do
    import Ecto.Query
    latest = Air.Schemas.Settings |> last() |> Air.Repo.one()
    latest || default()
  end

  defp unserialize_retention_days(nil), do: :unlimited
  defp unserialize_retention_days(days), do: days

  defp ldap_changeset(settings, params) do
    settings
    |> cast(params, @ldap_fields)
    |> validate_number(:ldap_port, greater_than_or_equal_to: 1)
  end

  defp changeset(settings, params \\ %{}) do
    settings
    |> cast(params, @required_fields ++ @optional_fields)
    |> validate_required(@required_fields)
    |> validate_length(:decimal_sep, is: 1)
    |> validate_length(:thousand_sep, max: 1)
    |> validate_number(:decimal_digits, greater_than_or_equal_to: 1, less_than_or_equal_to: 9)
  end

  defp default(),
    do: %Air.Schemas.Settings{
      query_retention_days: nil,
      audit_log_enabled: true,
      decimal_sep: ".",
      thousand_sep: " ",
      decimal_digits: 3,
      ldap_host: nil,
      ldap_port: nil,
      ldap_ssl: false,
      ldap_ca_cert: nil
    }
end
