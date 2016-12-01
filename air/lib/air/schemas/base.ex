defmodule Air.Schemas.Base do
  @moduledoc "Common boilerplate for schemas."

  @doc false
  defmacro __using__(_opts) do
    quote do
      use Ecto.Schema

      import Ecto
      import Ecto.Changeset
      import Ecto.Query, only: [from: 1, from: 2]
    end
  end
end
