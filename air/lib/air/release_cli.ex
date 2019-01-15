defmodule Air.ReleaseCLI do
  @moduledoc "Contains functions exposed to the system administrator via CLI commands."

  alias Air.Service

  @doc "Prints a password reset token for the user with the given login."
  @spec reset_password(String.t()) :: :ok
  def reset_password(login) do
    login
    |> to_string()
    |> Service.User.get_by_login()
    |> case do
      {:ok, user} ->
        token = Service.User.reset_password_token(user)
        IO.puts("Use the following token in the `Forgot password` form:\n\n#{token}\n")

      _ ->
        IO.puts("A user with that login does not exist.")
    end

    :ok
  end

  @doc """
  Takes a list of passwords and produces a list of password hashes.

  The output can be used to create a users and data source credentials file
  in order to statically configure Insights Air.
  """
  @spec hash_passwords([String.t()]) :: :ok
  def hash_passwords(passwords) do
    Application.load(:air)

    passwords
    |> Enum.map(&Service.Password.hash/1)
    |> Enum.join("\n")
    |> IO.puts()
  end
end
