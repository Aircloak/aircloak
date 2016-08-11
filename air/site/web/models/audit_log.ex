defmodule Air.AuditLog do
  @moduledoc "Model for recording events having taken place"
  use Air.Web, :model
  require Logger

  alias Air.{User, Repo}

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
  def changeset(model, params \\ :empty) do
    model
    |> cast(params, @required_fields, @optional_fields)
  end

  def metadata_items(log_entry) do
    Map.to_list(Poison.decode!(log_entry.metadata))
  end

  def for_display(log_entry) do
    metadata = Poison.decode!(log_entry.metadata)
    |> Map.to_list()
    |> Enum.map(fn({name, value}) -> [Phoenix.Naming.humanize(name), value] end)

    inserted_at = log_entry.inserted_at
    |> Ecto.DateTime.to_erl()
    |> Timex.DateTime.from_erl()
    |> Timex.from_now()

    %{
      user: log_entry.user,
      event: log_entry.event,
      metadata: metadata,
      inserted_at: inserted_at,
    }
  end


  # -------------------------------------------------------------------
  # Auditable events
  # -------------------------------------------------------------------

  @doc "Saves an audit log entry, but allows the operation to fail."
  @spec log(Plug.Conn.t, String.t, User.t, Keyword.t) :: :ok
  def log(%Plug.Conn{} = conn, event, options \\ [], user \\ nil) do
    user = case user do
      nil ->
        case conn.assigns.current_user do
          nil -> %{email: "Unknown user"}
          user -> user
        end
      _ -> user
    end

    peer = case conn.peer do
      {ip, port} -> "#{ip_to_string(ip)}:#{port}"
      _ -> "Unknown"
    end
    remote_ip = case conn.remote_ip do
      ip when is_tuple(ip) -> ip_to_string(ip)
      _ -> "Unknown"
    end

    defaults = %{
      peer: peer,
      remote_ip: remote_ip,
    }

    metadata = options
    |> Enum.into(%{})
    |> Map.merge(defaults)

    params = %{
      user: user.email,
      event: event,
      metadata: Poison.encode!(metadata),
    }
    case Repo.insert(changeset(%__MODULE__{}, params)) do
      {:ok, _} -> :ok
      {:error, _} ->
        Logger.error("Failed at storing audit log entry")
        :ok
    end
  end

  defp ip_to_string({a, b, c, d}), do: "#{a}.#{b}.#{c}.#{d}"
end
