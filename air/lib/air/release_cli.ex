defmodule Air.ReleaseCLI do
  @moduledoc "Contains functions exposed to the system administrator via CLI commands."

  alias Air.{Repo, Schemas, Service}

  @doc "Prints a password reset token for the user with the given login."
  @spec reset_password(String.t()) :: :ok
  def reset_password(login) do
    Schemas.User
    |> Repo.get_by(login: to_string(login))
    |> case do
      nil ->
        IO.puts("A user with that login does not exist.")

      user ->
        token = Service.User.reset_password_token(user)
        IO.puts("Use the following token in the `Forgot password` form:\n\n#{token}\n")
    end

    :ok
  end
end
