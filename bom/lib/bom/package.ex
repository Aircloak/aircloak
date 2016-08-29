defmodule BOM.Package do
  @moduledoc "A structure for keeping information about a single dependency."

  defstruct [:realm, :name, :license, :error]

  @type t :: %__MODULE__{realm: atom, name: String.t, license: BOM.License.t, error: nil | String.t}
end
