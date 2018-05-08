defmodule Air.ReleaseCLI do
  @moduledoc "Contains functions exposed to the system administrator via CLI commands."

  alias Air.{Repo, Schemas, Service}

  @doc "Prints a password reset token for the user with the given email."
  @spec reset_password(String.t()) :: :ok
  def reset_password(email) do
    Schemas.User
    |> Repo.get_by(email: to_string(email))
    |> case do
      nil ->
        IO.puts("A user with that email does not exist.")

      user ->
        token = Service.User.reset_password_token(user)
        IO.puts("Use the following token in the `Forgot password` form:\n\n#{token}\n")
    end

    :ok
  end
end
