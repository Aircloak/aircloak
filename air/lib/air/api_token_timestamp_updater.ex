defmodule Air.ApiTokenTimestampUpdater do
  @moduledoc "Service for asynchronous API token touching."


  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @doc "Starts the service."
  @spec start_link() :: Supervisor.on_start
  def start_link(), do:
    Task.Supervisor.start_link(name: __MODULE__)

  @doc "Starts a process which touches the API token."
  @spec start_token_toucher(Air.Schemas.ApiToken.t) :: :ok
  def start_token_toucher(token) do
    Task.Supervisor.start_child(Air.ApiTokenTimestampUpdater, fn() ->
      Air.Repo.update(Air.Schemas.ApiToken.touch(token))
    end)

    :ok
  end


  # -------------------------------------------------------------------
  # Supervision tree
  # -------------------------------------------------------------------

  @doc false
  def child_spec(_arg), do:
    %{
      id: __MODULE__, restart: :permanent, shutdown: :infinity, type: :supervisor,
      start: {__MODULE__, :start_link, []},
    }
end
