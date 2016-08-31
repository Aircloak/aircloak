defmodule Air.AuditLog do
  @moduledoc "Model for recording events having taken place"
  use Air.Web, :model
  require Logger

  alias Air.Repo

  schema "audit_logs" do
    field :event, :string
    field :user, :string
    field :metadata, :string

    timestamps
  end

  @required_fields ~w(event user metadata)
  @optional_fields ~w()

  @doc """
  Creates a changeset based on the `model` and `params`.

  If no params are provided, an invalid changeset is returned
  with no validation performed.
  """
  def changeset(model, params \\ %{}) do
    model
    |> cast(params, @required_fields, @optional_fields)
  end

  @doc "Converts a log entry model into a map that can be converted to JSON"
  @spec for_display(AuditLog.t) :: Map.t
  def for_display(log_entry) do
    metadata = Poison.decode!(log_entry.metadata)
    |> Map.to_list()
    |> Enum.map(fn({name, value}) -> [Phoenix.Naming.humanize(name), value] end)

    %{
      user: log_entry.user,
      event: log_entry.event,
      metadata: metadata,
      inserted_at: Air.Utils.DateTime.time_ago(log_entry.inserted_at),
    }
  end


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Saves an audit log entry, but allows the operation to fail."
  @spec log(Plug.Conn.t, String.t, Keyword.t) :: :ok
  def log(conn, event, options \\ []) do
    params = build_params(conn, event, options)
    case Repo.insert(changeset(%__MODULE__{}, params)) do
      {:ok, _} -> :ok
      {:error, _} ->
        Logger.error("Failed at storing audit log entry")
        :ok
    end
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp ip_to_string({a, b, c, d}), do: "#{a}.#{b}.#{c}.#{d}"

  defp user(conn) do
    case conn.assigns.current_user do
      nil -> %{email: "Unknown user"}
      user -> user
    end
  end

  defp peer(conn) do
    case conn.peer do
      {ip, port} -> "#{ip_to_string(ip)}:#{port}"
      _ -> "Unknown"
    end
  end

  defp remote_ip(conn) do
    case conn.remote_ip do
      ip when is_tuple(ip) -> ip_to_string(ip)
      _ -> "Unknown"
    end
  end

  defp build_params(conn, event, options) do
    defaults = %{
      peer: peer(conn),
      remote_ip: remote_ip(conn),
    }

    metadata = options
    |> Enum.into(%{})
    |> Map.merge(defaults)

    %{
      user: user(conn).email,
      event: event,
      metadata: Poison.encode!(metadata),
    }
  end
end
