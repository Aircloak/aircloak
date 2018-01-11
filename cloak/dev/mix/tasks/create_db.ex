defmodule Mix.Tasks.Cloak.CreateDb do
  @moduledoc "Creates databases from the given json configuration."
  @shortdoc @moduledoc

  # Mix.Task behaviour is not in PLT since Mix is not a runtime dep, so we disable the warning
  @dialyzer :no_undefined_callbacks

  use Mix.Task


  # -------------------------------------------------------------------
  # Mix task interface
  # -------------------------------------------------------------------

  @impl Mix.Task
  def run([config_name]) do
    config_name
    |> Compliance.DataSources.all_from_config()
    |> Enum.map(&Compliance.DataSources.create/1)
  end
end
