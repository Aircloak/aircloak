defmodule BOM do
  @moduledoc false
  use Application

  def start(_type, _opts) do
    Supervisor.start_link(children(), strategy: :one_for_one, name: BOM.Supervisor)
  end

  defp children do
    import Supervisor.Spec
    [
      worker(BOM.Gather.Elixir.Hex, [])
    ]
  end
end
