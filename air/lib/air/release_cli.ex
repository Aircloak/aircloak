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

  @doc """
  Takes username and password data as the first argument of the form:

    login1:password1
    login2:password2

  and outputs a string to STDOUT of the form:

    login1:password1-hash
    login2:password2-hash

  The output can later be given as an input to Insights Air as a
  list of statically configured users that should be added to the system.
  """
  @spec hash_credentials([String.t()]) :: :ok
  def hash_credentials(content) do
    Application.load(:air)

    credentials =
      content
      |> Enum.join("\n")
      |> Service.Password.process_credentials()
      |> Enum.map(&"#{&1.login}:#{&1.hash}")
      |> Enum.join("\n")

    IO.puts(:stdio, credentials)
  end
end
