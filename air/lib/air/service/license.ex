defmodule Air.Service.License do
  use GenServer

  alias __MODULE__.FSM
  alias Air.{Repo, Schemas}
  import Ecto.Query

  if Mix.env() == :test do
    def valid?(), do: true
  else
    def valid?(), do: GenServer.call(__MODULE__, :valid?)
  end

  def load(text), do: GenServer.call(__MODULE__, {:load, text})

  def expiry(), do: GenServer.call(__MODULE__, :expiry)

  def present?(), do: GenServer.call(__MODULE__, :present?)

  @impl GenServer
  def init(_) do
    public_key = load_public_key!()
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
        {:reply, :ok, %{state | fsm: fsm}}
      {:error, fsm} -> {:reply, :error, %{state | fsm: fsm}}
    end
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
    Schemas.License.changeset(%Schemas.License{}, %{text: text})
    |> Repo.insert!()

  @doc false
  def child_spec(_arg), do:
    Aircloak.ChildSpec.gen_server(__MODULE__, [], name: __MODULE__)
end
