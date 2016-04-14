defmodule Aircloak.SyncRequester.Backend do
    @moduledoc """
    Behaviour for `Aircloak.SyncRequester` backends.

    A backend is used to store pending requests and later match them to corresponding
    responses.
    """

    alias Aircloak.SyncRequester

    @doc "Stores the request data to the backend."
    @callback store_request(
          SyncRequester.backend_arg,
          SyncRequester.request_id,
          SyncRequester.request,
          SyncRequester.meta
        ) :: :ok | :error | {:error, any}

    @doc """
    Pops the request data from the backend.

    The implementation is expected to atomically retrieve and delete the request
    from the backend.

    It is possible that the request is not stored in the backend. In such cases,
    the function should return `{:error, reason}`.
    """
    @callback pop_request(SyncRequester.backend_arg, SyncRequester.request_id) ::
        {:matched, {SyncRequester.request, SyncRequester.meta}} |
        {:error, any}
  end

defmodule Aircloak.SyncRequester.Backend.Ets do
  @moduledoc """
  ETS powered backend for `Aircloak.SyncRequester`.

  To use this backend, you need to start the owner process with `start_link/1`.
  The function takes the `requester_name`, which is an atom uniquely identifying
  the backend in the system.

  Then, you can use `Aircloak.SyncRequester` functions, passing the `requester_name`
  as the backend argument.
  """

  @behaviour Aircloak.SyncRequester.Backend
  use GenServer


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Starts the owner process of the ETS table."
  @spec start_link(atom) :: GenServer.on_start
  def start_link(requester_name) do
    GenServer.start_link(__MODULE__, requester_name)
  end


  # -------------------------------------------------------------------
  # Aircloak.SyncRequester.Backend callbacks
  # -------------------------------------------------------------------

  @doc false
  def store_request(requester_name, id, request, meta) do
    if :ets.insert_new(table(requester_name), {id, {request, meta}}) == true do
      :ok
    else
      {:error, :already_exists}
    end
  end

  @doc false
  def pop_request(requester_name, id) do
    case :ets.take(table(requester_name), id) do
      [{^id, {request, meta}}] ->
        {:ok, request, meta}
      [] ->
        {:error, :not_found}
    end
  end


  # -------------------------------------------------------------------
  # GenServer callbacks
  # -------------------------------------------------------------------

  @doc false
  def init(requester_name) do
    :ets.new(table(requester_name), [
          :set,
          :public,
          :named_table,
          write_concurrency: true,
          read_concurrency: true
        ])

    {:ok, nil}
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp table(requester_name), do: Module.concat(__MODULE__, requester_name)
end
