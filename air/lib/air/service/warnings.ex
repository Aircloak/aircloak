defmodule Air.Service.Warnings do
  @moduledoc "A service providing warnings about problems in the system"

  @type problem :: %{
    resource: any,
    description: String.t,
    severity: :high | :medium | :low,
  }

  alias Air.Service.{DataSource, Cloak}


  #-----------------------------------------------------------------------------------------------------------
  # API functions
  #-----------------------------------------------------------------------------------------------------------

  @doc "Whether or not there are any known problems"
  @spec known_problems?() :: boolean
  def known_problems?(), do: problems() !== []

  @doc """
  Returns a list of problems. The list contains a structured
  set of problems, containing the name of the resource that is having a problem,
  a problem description, along with a link leading to the affected resource.
  """
  @spec problems() :: [problem]
  def problems(), do: data_source_problems()


  #-----------------------------------------------------------------------------------------------------------
  # Internal functions
  #-----------------------------------------------------------------------------------------------------------

  defp data_source_problems() do
    data_sources = DataSource.all()
    offline_datasources(data_sources)
      ++ broken_datasources(data_sources)
      ++ no_group(data_sources)
      ++ no_users(data_sources)
  end

  defp problem(resource, description, severity \\ :low), do:
    %{resource: resource, description: description, severity: severity}

  defp offline_datasources(data_sources), do:
    data_sources
    |> Enum.filter(fn(data_source) -> Cloak.channel_pids(data_source.name) == [] end)
    |> Enum.map(
      &problem(&1, "The data source is unavailable. No cloaks serving this data source are online", :high)
    )

  defp broken_datasources(data_sources), do:
    data_sources
    |> Enum.reject(&(&1.errors === "" or &1.errors === "[]"))
    |> Enum.map(&problem(&1, unpack_error(&1), :medium))

  defp unpack_error(data_source), do:
    data_source.errors
    |> Poison.decode!()
    |> Enum.join(", ")

  defp no_group(data_sources), do:
    data_sources
    |> Enum.reject(&(length(&1.groups) > 0))
    |> Enum.map(&problem(&1, "No groups have been given access to the data source. It cannot be queried"))

  defp no_users(data_sources), do:
    data_sources
    |> Enum.reject(fn(data_source) ->
      Enum.any?(data_source.groups, fn(group) ->
        group = Air.Repo.preload(group, [:users])
        length(group.users) > 0
      end)
    end)
    |> Enum.map(&problem(&1, "No users have access to this data source"))
end
