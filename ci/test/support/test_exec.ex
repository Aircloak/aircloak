defmodule AircloakCI.TestExec do
  @moduledoc false

  def run_link(_cmd, _opts) do
    {:ok, pid} = Task.start_link(fn -> :ok end)
    {:ok, pid, nil}
  end

  def stop(_pid) do
    :ok
  end
end
