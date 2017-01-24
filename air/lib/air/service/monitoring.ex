defmodule Air.Service.Monitoring do
  import Ecto.Query

  alias Air.{Repo, Schemas.Group}

  def assemble_info() do
    %{
      groups: fetch_group_names()
    }
  end

  defp fetch_group_names() do
    Group |> select([g], g.name) |> Repo.all()
  end
end
