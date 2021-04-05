defmodule Air.Service.Settings do
  @moduledoc "Services for reading and writing Air-wide options."

  use GenServer

  import Ecto.Changeset

  @required_fields ~w(audit_log_enabled decimal_sep decimal_digits type_checking_enabled)a
  @optional_fields ~w(query_retention_days thousand_sep login_message main_message)a

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Returns the current version of the settings."
  @spec read() :: Air.Settings.t()
  def read(server \\ __MODULE__), do: GenServer.call(server, :read)

  @doc "Saves the specified settings."
  @spec save(%{optional(atom) => any()}) :: {:ok, Air.Schemas.Settings.t()} | {:error, Ecto.Changeset.t()}
  def save(server \\ __MODULE__, params), do: GenServer.call(server, {:save, params})

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
    latest_settings()
    |> changeset(params)
    |> do_save()
    |> case do
      {:ok, updated} -> {:reply, {:ok, parse(updated)}, updated}
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
      login_message: schema.login_message,
      main_message: schema.main_message,
      type_checking_enabled: schema.type_checking_enabled
    }
  end

  defp latest_settings() do
    import Ecto.Query
    latest = Air.Schemas.Settings |> last() |> Air.Repo.one()
    latest || default()
  end

  defp unserialize_retention_days(nil), do: :unlimited
  defp unserialize_retention_days(days), do: days

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
      login_message: nil,
      main_message: nil,
      type_checking_enabled: true
    }
end
