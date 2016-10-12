defmodule Air.Service.User do
  @moduledoc "Service module for working with users"

  alias Air.{DataSource, DataSourceManager, Repo, User}
  alias Air.Service.AuditLog


  #-----------------------------------------------------------------------------------------------------------
  # API functions
  #-----------------------------------------------------------------------------------------------------------

  @doc """
  Authenticates the given user, and optionally verifies if it can work with the given data source.

  If data source name is provided, it has to be available to the given user.
  """
  @spec login(String.t, String.t, nil | String.t, Keyword.t) :: User.t | nil
  def login(email, password, data_source_name, meta \\ []) do
    user = Repo.get_by(User, email: email)
    if User.validate_password(user, password) && can_login_to_data_source?(user, data_source_name) do
      AuditLog.log(user, "Logged in", meta)
      user
    else
      AuditLog.log(user, "Failed login", meta)
      nil
    end
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
