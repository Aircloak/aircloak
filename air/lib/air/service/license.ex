defmodule Air.Service.License do
  use GenServer

  alias __MODULE__.FSM

  def valid?(), do: GenServer.call(__MODULE__, :valid?)

  def load(text), do: GenServer.call(__MODULE__, {:load, text})

  def expiry(), do: GenServer.call(__MODULE__, :expiry)

  def present?(), do: GenServer.call(__MODULE__, :present?)

  @impl GenServer
  def init(_), do:
    {:ok, %{public_key: load_public_key!(), fsm: FSM.initial()}}

  @impl GenServer
  def handle_call(:valid?, _from, state), do:
    {:reply, FSM.valid?(state.fsm), state}
  def handle_call({:load, text}, _from, state) do
    {result, fsm} = FSM.load(state.fsm, state.public_key, text)
    {:reply, result, %{state | fsm: fsm}}
  end
  def handle_call(:expiry, _from, state), do:
    {:reply, FSM.expiry(state.fsm), state}
  def handle_call(:present?, _from, state), do:
    {:reply, FSM.present?(state.fsm), state}

  defp load_public_key!() do
    root_path = Application.app_dir(:air)
    file_name = Application.get_env(:air, :license) |> Keyword.fetch!(:public_key)
    {:ok, public_key} = ExPublicKey.load(Path.join([root_path, file_name]))
    public_key
  end

  @doc false
  def child_spec(_arg), do:
    Aircloak.ChildSpec.gen_server(__MODULE__, [], name: __MODULE__)
end
