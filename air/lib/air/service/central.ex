defmodule Air.Service.Central do
  @moduledoc "Service functions related to central calls."

  alias Aircloak.ChildSpec

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc false
  def child_spec(_arg) do
    children = [ChildSpec.registry(:unique, Air.Service.Central.Registry)]
    ChildSpec.supervisor(children, strategy: :one_for_one, name: __MODULE__)
  end
end
