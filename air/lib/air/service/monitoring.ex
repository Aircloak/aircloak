defmodule Air.Service.Monitoring do
  import Ecto.Query

  alias Air.{Repo, Schemas.Group}

  def assemble_info() do
    %{
      uptime: fetch_uptime(),
      groups: fetch_group_names(),
    }
  end

  defp fetch_group_names(), do:
    Group |> select([g], g.name) |> Repo.all()

  defp fetch_uptime() do
    {total, _since_last_call} = :erlang.statistics(:wall_clock)
    total
  end
end
