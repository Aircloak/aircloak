defmodule Air.Schemas.Login do
  @moduledoc "The login model."

  use Air.Schemas.Base

  @type t :: %__MODULE__{}

  schema "logins" do
    belongs_to(:user, Air.Schemas.User)

    field(:login, :string)
    field(:hashed_password, :string)
  end

  defimpl Inspect do
    @moduledoc """
    Custom inspection of the login record, where we only present non-sensitive fields.

    This allows us to safely inspect login anywhere in log expressions without
    worrying we'll leek some sensitive data.
    """
    def inspect(login, opts) do
      Inspect.Map.inspect(Map.take(login, [:id]), inspect(login.__struct__), opts)
    end
  end
end
