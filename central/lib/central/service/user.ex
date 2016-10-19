defmodule Central.Service.User do
  @moduledoc "Service module for working with users"

  alias Central.{Repo, User}

  #-----------------------------------------------------------------------------------------------------------
  # API functions
  #-----------------------------------------------------------------------------------------------------------

  @doc "Authenticates the given user."
  @spec login(String.t, String.t) ::
    {:ok, User.t} | {:error, :invalid_email_or_password}
  def login(email, password) do
    user = Repo.get_by(User, email: email)
    with :ok <- validate_password(user, password) do
      {:ok, user}
    else
      error -> error
    end
  end


  #-----------------------------------------------------------------------------------------------------------
  # Internal functions
  #-----------------------------------------------------------------------------------------------------------

  defp validate_password(user, password), do:
    if User.validate_password(user, password),
      do: :ok,
      else: {:error, :invalid_email_or_password}
end
