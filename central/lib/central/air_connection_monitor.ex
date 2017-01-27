defmodule Central.AirConnectionMonitor do
  @moduledoc "Monitoring of air connections."

  use GenServer

  @registry_name Module.concat(__MODULE__, Registry)
  @task_sup_name Module.concat(__MODULE__, TaskSup)
  @monitor_sup_name Module.concat(__MODULE__, MonitorSup)


  # -------------------------------------------------------------------
  # Api functions
  # -------------------------------------------------------------------

  @doc "Starts the supervisor of connection monitors."
  @spec start_link() :: Supervisor.on_start
  def start_link() do
    import Supervisor.Spec, warn: false

    Supervisor.start_link(
      [
        worker(Registry, [:unique, @registry_name]),
        supervisor(Task.Supervisor, [[name: @task_sup_name]]),
        supervisor(Supervisor, [
          [worker(GenServer, [__MODULE__], restart: :temporary)],
          [id: @monitor_sup_name, name: @monitor_sup_name, strategy: :simple_one_for_one]
        ])
      ],
      strategy: :rest_for_one
    )
  end

  @doc "Should be started from the air channel process to monitor the connection."
  @spec monitor_channel(Central.Schemas.Customer.t, String.t) :: :ok
  def monitor_channel(customer, air_name) do
    {:ok, _} =
      Supervisor.start_child(@monitor_sup_name, [
        {customer, air_name, self()},
        [name: {:via, Registry, {@registry_name, {customer.id, air_name}}}]
      ])
    :ok
  end


  # -------------------------------------------------------------------
  # GenServer callbacks
  # -------------------------------------------------------------------

  @doc false
  def init({customer, air_name, channel_pid}) do
    state = %{
      customer: customer,
      air_name: air_name,
      channel_pid: channel_pid,
      mref: Process.monitor(channel_pid)
    }
    update_air_status(state, :online)
    {:ok, state}
  end

  @doc false
  def handle_info({:DOWN, mref, :process, channel_pid, _}, %{channel_pid: channel_pid, mref: mref} = state) do
    update_air_status(state, :offline)
    {:stop, :normal, %{state | channel_pid: nil, mref: nil}}
  end
  def handle_info(_, state), do:
    {:noreply, state}


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp update_air_status(state, status) do
    Task.Supervisor.start_child(@task_sup_name, Central.Service.Customer, :update_air_status,
      [state.customer, state.air_name, status])

    Task.Supervisor.start_child(@task_sup_name, Central.Service.ElasticSearch, :record_air_presence,
      [state.customer, state.air_name, status])
  end
end
