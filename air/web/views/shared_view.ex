defmodule Air.SharedView do
  @moduledoc false;
  use Air.Web, :view
  # bug in the current Phoenix
  @dialyzer :no_match

  def version() do
    :air
    |> Aircloak.Version.for_app()
    |> Aircloak.Version.to_string()
  end
end
