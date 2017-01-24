defmodule Air.Service.Monitoring do
  import Ecto.Query

  alias Air.{Repo, Schemas.Group, Schemas.User}

  def assemble_info() do
    %{
      uptime: fetch_uptime(),
      groups: fetch_group_names(),
      users: fetch_users(),
    }
  end

  defp fetch_group_names(), do:
    Group |> select([g], g.name) |> Repo.all()

  defp fetch_uptime() do
    {total, _since_last_call} = :erlang.statistics(:wall_clock)
    total
  end

  defp fetch_users() do
    for user <- User |> preload(:groups) |> Repo.all() do
      %{
        name: user.name,
        email: user.email,
        groups: user.groups |> Enum.map(&(&1.name))
      }
    end
  end
end
