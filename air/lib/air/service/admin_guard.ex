defmodule Air.Service.AdminGuard do
  @moduledoc "Contains functions that help with ensuring the last system administrator is not deleted."

  alias Air.Repo

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc """
  Runs the function, making sure the last admin is not deleted.

  The function is run in a transaction that is rolled back if no admin remains after the changes are applied.
  """
  @spec commit_if_active_last_admin((() -> t)) :: {:ok, t} | {:error, :forbidden_no_active_admin} | {:error, any}
        when t: var
  def commit_if_active_last_admin(fun), do: GenServer.call(__MODULE__, {:commit_if_active_last_admin, fun})

  @doc """
  Runs the function asynchronously, making sure the last admin is not deleted

  The function is run in a transaction that is rolled back if no admin remains after the changes are applied. If the
  changes are successful, `success_callback` is called with no arguments. If they are not, `failure_callback` is called
  with the error reason.
  """
  @spec commit_if_active_last_admin_async((() -> any), (() -> any), (any -> any)) :: :ok
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
