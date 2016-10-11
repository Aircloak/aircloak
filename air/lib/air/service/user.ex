defmodule Air.Service.User do
  @moduledoc "Service module for working with users"

  alias Air.{DataSource, DataSourceManager, Repo, User}


  #-----------------------------------------------------------------------------------------------------------
  # API functions
  #-----------------------------------------------------------------------------------------------------------

  @doc """
  Authenticates the given user, and optionally verifies if it can work with the given data source.

  If data source name is provided, it has to be available to the given user.

  If the authentication fails, the result will contain the user struct, if such user exists.
  This can be used to audit failed login attempts.
  """
  @spec login(String.t, String.t, nil | String.t) :: {:ok, User.t} | {:error, User.t | nil}
  def login(email, password, data_source_name \\ nil) do
    user = Repo.get_by(User, email: email)
    if User.validate_password(user, password) && can_login_to_data_source?(user, data_source_name),
      do: {:ok, user},
      else: {:error, user}
  end

  @doc "Returns data sources visible to the given user."
  @spec data_sources(User.t) :: DataSource.t
  def data_sources(user), do:
    user
    |> DataSource.for_user()
    |> Repo.all()


  #-----------------------------------------------------------------------------------------------------------
  # Internal functions
  #-----------------------------------------------------------------------------------------------------------

  defp can_login_to_data_source?(_user, nil), do: true
  defp can_login_to_data_source?(user, name), do:
    user
    |> data_sources()
    |> Enum.filter(&(DataSourceManager.available?(&1.global_id)))
    |> Enum.any?(&(&1.name == name))
end
