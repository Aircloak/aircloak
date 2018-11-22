defmodule Air.Repo.Seeder do
  @moduledoc "Used in development and tests to seed the database."

  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @doc "Seeds the database with the development data."
  @spec seed() :: :ok
  def seed() do
    {:ok, admin} = Air.Service.User.get_by_login("admin@aircloak.com")
    create_admin_token!(admin)

    :ok
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  if Mix.env() != :test do
    defp create_admin_token!(admin) do
      token = Air.Service.Token.create_api_token(admin, :api, "development admin token")
      file_name = Path.join(~w(#{Application.app_dir(:air)} priv dev admin_token))
      file_name |> Path.dirname() |> File.mkdir_p!()
      File.write!(file_name, token)
    end
  else
    defp create_admin_token!(_admin), do: :ok
  end
end
