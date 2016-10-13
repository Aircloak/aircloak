defmodule Air.Service.User do
  @moduledoc "Service module for working with users"

  alias Air.{DataSourceManager, Repo, User}
  alias Air.Service.{AuditLog, DataSource}


  #-----------------------------------------------------------------------------------------------------------
  # API functions
  #-----------------------------------------------------------------------------------------------------------

  @doc """
  Authenticates the given user, and optionally verifies if it can work with the given data source.

  If data source name is provided, it has to be available to the given user.
  """
  @spec login(String.t, String.t, nil | String.t, %{atom => any}) ::
    {:ok, User.t} | {:error, :invalid_email_or_password | :unauthorized}
  def login(email, password, data_source_name, meta \\ %{}) do
    user = Repo.get_by(User, email: email)
    with :ok <- validate_password(user, password),
         :ok <- validate_data_source_access(user, data_source_name) do
      AuditLog.log(user, "Logged in", meta)
      {:ok, user}
    else
      error ->
        AuditLog.log(user, "Failed login", meta)
        error
    end
  end


  #-----------------------------------------------------------------------------------------------------------
  # Internal functions
  #-----------------------------------------------------------------------------------------------------------

  defp validate_password(user, password), do:
    if User.validate_password(user, password),
      do: :ok,
      else: {:error, :invalid_email_or_password}

  defp validate_data_source_access(_user, nil), do: :ok
  defp validate_data_source_access(user, name), do:
    if (
      user
      |> DataSource.for_user()
      |> Enum.filter(&(DataSourceManager.available?(&1.global_id)))
      |> Enum.any?(&(&1.name == name))
    ),
      do: :ok,
      else: {:error, :unauthorized}
end
