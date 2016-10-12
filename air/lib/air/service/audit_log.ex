defmodule Air.Service.AuditLog do
  @moduledoc "Services for using the audit log."

  alias Air.{Repo, AuditLog}

  require Logger

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
end
