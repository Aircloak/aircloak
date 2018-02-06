defmodule Air.ProcessQueue do
  @moduledoc """
  Basic FIFO process queue.

  This module powers a process queue which can be used to limit the amount of concurrency for some operations. See
  `child_spec/1` and `run/4` for more details.

  Internally, the module is powered by `poolboy`. A better choice would be to use something like `jobs`, or
  `safetyvalve`, but `poolboy` was chosen because it's already a dependency, and it does a decent job up to some point.
  """

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc """
  Puts the operation into the queue, waits for the operation to be executed, and returns its result.

  The operation is executed inside the caller process, so there's no message passing involved.

  The `timeout` parameter can be used to specify the amount of time to wait for the operation to start
  executing. The running time of the operation is not accounted for. Once it starts running, it can
  run forever.
  """
  @spec run(atom, (() -> result), pos_integer | :infinity) :: result when result: var
  def run(pool_name, fun, timeout \\ :infinity), do:
    :poolboy.transaction(pool_name, fn(_worker) -> fun.() end, timeout)


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defmodule Worker do
    @moduledoc false

    # Dummy worker module which we need for dummy poolboy processes. We're not really using these
    # processes, but we need it so we can piggyback on poolboy's queue logic.

    def start_link(_), do: Agent.start_link(fn -> :ok end)
  end


  # -------------------------------------------------------------------
  # Supervision tree
  # -------------------------------------------------------------------

  @doc false
  def child_spec({name, size}), do:
    :poolboy.child_spec(
      name,
      name: {:local, name},
      worker_module: __MODULE__.Worker,
      max_overflow: 0,
      size: size
    )
end
