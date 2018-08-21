defmodule Air.Repo.Seeder do
  @moduledoc "Used in development and tests to seed the database."

  alias Air.Service.User

  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @doc "Seeds the database with the development data."
  @spec seed() :: :ok
  def seed() do
    admin = create_admin!()
    create_admin_token!(admin)
    create_plain_user!()

    :ok
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp admin_group() do
    case User.admin_groups() do
      [] -> User.create_group!(%{name: "admin", admin: true})
      [group | _] -> group
    end
  end

  defp create_admin!() do
    {:ok, admin} =
      %{
        login: "admin@aircloak.com",
        name: "Aircloak test administrator",
        groups: [admin_group().id]
      }
      |> User.create!()
      |> User.reset_password_token()
      |> User.reset_password(%{password: "password1234", password_confirmation: "password1234"})

    admin
  end

  defp create_plain_user!() do
    User.create!(%{
      login: "user@aircloak.com",
      name: "Test client regular user"
    })
    |> User.reset_password_token()
    |> User.reset_password(%{password: "password1234", password_confirmation: "password1234"})
  end

  if Mix.env() != :test do
    defp create_admin_token!(admin) do
      token = Air.Token.create_api_token(admin, :api, "development admin token")
      file_name = Path.join(~w(#{Application.app_dir(:air)} priv dev admin_token))
      file_name |> Path.dirname() |> File.mkdir_p!()
      File.write!(file_name, token)
    end
  else
    defp create_admin_token!(_admin), do: :ok
  end
end
