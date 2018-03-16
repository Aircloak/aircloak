defmodule Air.Service.License do
  @moduledoc """
  Maintains the single instance of a license for the system. Forwards its logic to Air.Service.License.FSM.
  (De)serializes the license from/to the database as needed.
  """

  use GenServer

  alias __MODULE__.{FSM, Key}
  alias Air.{Repo, Schemas, CentralClient}
  import Ecto.Query


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Returns true if the system license is valid, false otherwise."
  def valid?(), do: GenServer.call(__MODULE__, :valid?)

  @doc "Tries to load the given license text as the system license."
  @spec load(String.t) :: :ok | :error
  def load(text), do: GenServer.call(__MODULE__, {:load, text})

  @doc "Returns the expiry time for the system license."
  @spec expiry() :: DateTime.t
  def expiry(), do: GenServer.call(__MODULE__, :expiry)

  @doc "Returns true if a system license has ever been loaded, false otherwise."
  @spec present?() :: boolean
  def present?(), do: GenServer.call(__MODULE__, :present?)

  @doc "Calls central to renew the system license."
  @spec renew() :: :ok
  def renew(), do: GenServer.cast(__MODULE__, :renew)


  # -------------------------------------------------------------------
  # GenServer callbacks
  # -------------------------------------------------------------------

  @impl GenServer
  def init(_) do
    public_key = Key.public_key()
    {_res, fsm} = FSM.initial() |> FSM.load(public_key, read_from_db())
    {:ok, %{public_key: public_key, fsm: fsm}}
  end

  @impl GenServer
  def handle_call(:valid?, _from, state), do:
    {:reply, FSM.valid?(state.fsm), state}
  def handle_call({:load, text}, _from, state) do
    case FSM.load(state.fsm, state.public_key, text) do
      {:ok, fsm} ->
        save_to_db(text)
        renew()
        {:reply, :ok, %{state | fsm: fsm}}
      {:error, fsm} -> {:reply, :error, %{state | fsm: fsm}}
    end
  end
  def handle_call(:expiry, _from, state), do:
    {:reply, FSM.expiry(state.fsm), state}
  def handle_call(:present?, _from, state), do:
    {:reply, FSM.present?(state.fsm), state}

  @impl GenServer
  def handle_cast(:renew, state) do
    with \
         true <- CentralClient.Socket.connected?(),
         {:ok, text} <- state.fsm |> FSM.text() |> CentralClient.Socket.renew_license(),
         {:ok, fsm} <- FSM.load(state.fsm, state.public_key, text)
    do
      save_to_db(text)
      {:noreply, %{state | fsm: fsm}}
    else
      _ -> {:noreply, state}
    end
  end


  # -------------------------------------------------------------------
  # Private functions
  # -------------------------------------------------------------------

  defp read_from_db() do
    Schemas.License
    |> last()
    |> Repo.one()
    |> case do
      nil -> ""
      license -> license.text
    end
  end

  defp save_to_db(text), do:
    %Schemas.License{}
    |> Schemas.License.changeset(%{text: text})
    |> Repo.insert!()


  # -------------------------------------------------------------------
  # Supervision tree
  # -------------------------------------------------------------------

  @doc false
  def child_spec(_arg), do:
    Aircloak.ChildSpec.gen_server(__MODULE__, [], name: __MODULE__)
end
