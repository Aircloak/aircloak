defmodule Air.Service.AuditLog do
  @moduledoc "Services for using the audit log."

  alias Air.{Repo, AuditLog}
  import Ecto.Query, only: [from: 2]
  require Logger


  #-----------------------------------------------------------------------------------------------------------
  # API functions
  #-----------------------------------------------------------------------------------------------------------

  @doc "Creates an audit log entry."
  @spec log(nil | Air.User.t, String.t, %{atom => any}) :: :ok | {:error, any}
  def log(user, event, metadata \\ %{}) do
    email = if user != nil, do: user.email, else: "Unknown user"

    %AuditLog{}
    |> AuditLog.changeset(%{user: email, event: event, metadata: metadata})
    |> Repo.insert()
    |> case do
          {:ok, _} -> :ok
          {:error, reason} ->
            Logger.error("Failed at storing audit log entry: #{inspect(reason)}")
            {:error, reason}
        end
  end

  @doc """
  Returns audit log entries created in a given time interval (inclusive).

  Returned entries are descending sorted by the creation date.
  """
  @spec for(Map.t) :: [Air.AuditLog.t]
  def for(params) do
    AuditLog
    |> date_range(params.from, params.to)
    |> order_by_event()
    |> Repo.paginate(page: params.page)
  end

  #-----------------------------------------------------------------------------------------------------------
  # Internal functions
  #-----------------------------------------------------------------------------------------------------------

  defp order_by_event(query) do
    from a in query,
    order_by: [desc: :inserted_at]
  end

  defp date_range(query, from, to) do
    from a in query,
    where: a.inserted_at >= ^from and a.inserted_at <= ^to
  end
end
