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
    |> AuditLog.changeset(%{user: email, event: event, metadata: Poison.encode!(metadata)})
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
  @spec between(Ecto.DateTime.t, Ecto.DateTime.t) :: [Air.AuditLog.t]
  def between(from, to) do
    Repo.all(
      from a in AuditLog,
      where: a.inserted_at >= ^from and a.inserted_at <= ^to,
      order_by: [desc: :inserted_at]
    )
  end
end
