defmodule Air.Service.User.LoginStats do
  use GenServer

  @type event_type :: :tokens_revoked | :wrong_credentials | :unknown_login | :successful_login

  @type login_event :: %{
          type: :success | :failure,
          description: String.t(),
          login: String.t(),
          time: DateTime.t()
        }

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @spec log_event(String.t(), event_type()) :: :ok
  def log_event(login, event_type),
    do: GenServer.cast(__MODULE__, {:log_event, login, event_type})

  @doc "Returns the login attempts, whether failed or successful of the last hour"
  @spec get_stats() :: [login_event]
  def get_stats(),
    do: GenServer.call(__MODULE__, :get_stats)

  # -------------------------------------------------------------------
  # GenServer Callbacks
  # -------------------------------------------------------------------

  @impl GenServer
  def init(_) do
    schedule_cleanup()
    {:ok, %{events: []}}
  end

  @impl GenServer
  def handle_call(:get_stats, _from, state), do: {:reply, state.events, state}

  @impl GenServer
  def handle_cast({:log_event, login, event_type}, state) do
    [status, description] =
      case event_type do
        :successful_login -> [:success, "Login successful"]
        :tokens_revoked -> [:success, "All session tokens revoked"]
        :unknown_login -> [:failure, "Unknown login"]
        :wrong_credentials -> [:failure, "Wrong credentials provided"]
      end

    event = %{
      type: status,
      description: description,
      login: login,
      time: DateTime.utc_now()
    }

    all_events = [event | state.events]

    AirWeb.Endpoint.broadcast!("login_events", "update", %{login_events: all_events})

    {:noreply, %{state | events: all_events}}
  end

  @impl GenServer
  def handle_info(:cleanup, state) do
    schedule_cleanup()
    one_hour_ago = DateTime.add(DateTime.utc_now(), -3600, :second)
    {:noreply, %{state | events: state.events |> Enum.filter(&(&1.time > one_hour_ago))}}
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp schedule_cleanup(), do: Process.send_after(self(), :cleanup, :timer.minutes(1))

  # -------------------------------------------------------------------
  # Supervision tree
  # -------------------------------------------------------------------

  @doc false
  def child_spec(_arg), do: Aircloak.ChildSpec.gen_server(__MODULE__, [], name: __MODULE__)
end
