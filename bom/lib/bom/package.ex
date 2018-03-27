defmodule BOM.Package do
  @moduledoc "A structure for keeping information about a single dependency."

  defstruct [:realm, :name, :path, :license, :error, :version]

  @type t :: %__MODULE__{
          realm: atom,
          name: String.t(),
          path: String.t(),
          license: BOM.License.t(),
          error: nil | String.t(),
          version: String.t()
        }
end
