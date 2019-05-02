defmodule Air.Service.AdminGuard do
  alias Air.Repo

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  def commit_if_active_last_admin(fun), do: GenServer.call(__MODULE__, {:commit_if_active_last_admin, fun})

  def commit_if_active_last_admin_async(fun, success_callback, failure_callback),
    do: GenServer.cast(__MODULE__, {:commit_if_active_last_admin, fun, success_callback, failure_callback})

  # -------------------------------------------------------------------
  # GenServer callbacks
  # -------------------------------------------------------------------

  use GenServer

  @impl GenServer
  def init(_), do: {:ok, nil}

  @impl GenServer
  def handle_call({:commit_if_active_last_admin, fun}, _from, state),
    do: {:reply, do_commit_if_retains_an_admin(fun), state}

  @impl GenServer
  def handle_cast({:commit_if_active_last_admin, fun, success_callback, failure_callback}, state) do
    case do_commit_if_retains_an_admin(fun) do
      {:ok, _} -> success_callback.()
      {:error, error} -> failure_callback.(error)
    end

    {:noreply, state}
  end

  # -------------------------------------------------------------------
  # Private functions
  # -------------------------------------------------------------------

  defp do_commit_if_retains_an_admin(fun) do
    Repo.transaction(
      fn ->
        case fun.() do
          {:ok, result} ->
            if Air.Service.User.active_admin_user_exists?() do
              result
            else
              Repo.rollback(:forbidden_no_active_admin)
            end

          {:error, error} ->
            Repo.rollback(error)
        end
      end,
      timeout: :timer.hours(1)
    )
  end

  # -------------------------------------------------------------------
  # Supervision tree
  # -------------------------------------------------------------------

  @doc false
  def child_spec(_arg), do: Aircloak.ChildSpec.gen_server(__MODULE__, [], name: __MODULE__)
end
