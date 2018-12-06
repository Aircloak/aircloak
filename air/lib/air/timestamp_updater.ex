defmodule Air.TimestampUpdater do
  @moduledoc "Service for asynchronous API token touching."

  use Aircloak.ChildSpec.Supervisor

  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @doc "Starts the service."
  @spec start_link() :: Supervisor.on_start()
  def start_link(), do: Task.Supervisor.start_link(name: __MODULE__)

  @doc """
  Starts a process which touches the model. Does nothing in test, as starting an async task that uses the DB plays badly
  with the Ecto Sandbox.
  """
  @spec start_toucher(Ecto.Model.t()) :: :ok
  if Mix.env() == :test do
    def start_toucher(_model), do: :ok
  else
    def start_toucher(model) do
      Task.Supervisor.start_child(__MODULE__, fn ->
        model
        |> Ecto.Changeset.change(%{updated_at: NaiveDateTime.utc_now()})
        |> Air.Repo.update()
      end)

      :ok
    end
  end
end
