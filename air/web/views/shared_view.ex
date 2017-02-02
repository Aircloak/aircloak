defmodule Air.SharedView do
  @moduledoc false;
  use Air.Web, :view
  # bug in the current Phoenix
  @dialyzer :no_match

  def version(), do: Air.Mixfile.version()

  def release_name(), do: Air.Mixfile.release_name()
end
