defmodule Air.Schemas.Login do
  @moduledoc "The login model."

  use Air.Schemas.Base
  require EctoEnum

  @type t :: %__MODULE__{
          login: String.t(),
          hashed_password: String.t(),
          login_type: __MODULE__.LoginType
        }

  EctoEnum.defenum(LoginType, :login_type, [:main, :psql])

  schema "logins" do
    belongs_to(:user, Air.Schemas.User)

    field(:login, :string)
    field(:hashed_password, :string)
    field(:login_type, __MODULE__.LoginType)

    # These virtual fields are used for validation,
    # but never persisted to the database
    field(:password, :string, virtual: true)
    field(:password_confirmation, :string, virtual: true)

    timestamps()
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
