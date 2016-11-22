defmodule Air.Service.User do
  @moduledoc "Service module for working with users"

  alias Air.{Repo, Service.AuditLog, User}
  import Ecto.Query, only: [from: 2]

  #-----------------------------------------------------------------------------------------------------------
  # API functions
  #-----------------------------------------------------------------------------------------------------------

  @doc "Authenticates the given user."
  @spec login(String.t, String.t, %{atom => any}) ::
    {:ok, User.t} | {:error, :invalid_email_or_password}
  def login(email, password, meta \\ %{}) do
    user = Repo.get_by(User, email: email)
    if User.validate_password(user, password) do
      AuditLog.log(user, "Logged in", meta)
      {:ok, user}
    else
      AuditLog.log(user, "Failed login", meta)
      {:error, :invalid_email_or_password}
    end
  end

  @doc "Returns a list of all users in the system"
  @spec all() :: [User.t]
  def all() do
    Repo.all(User)
  end

  @doc "Given a list of email addresses, loads the corresponding users"
  @spec by_emails([String.t]) :: [User.t]
  def by_emails(emails) do
    Repo.all(from user in User,
      where: user.email in ^emails)
  end
end
