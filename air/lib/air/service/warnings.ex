defmodule Air.Service.Warnings do
  @moduledoc "A service providing warnings about problems in the system"

  @type problem :: %{
    resource: any,
    description: String.t,
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
    offline = offline_datasources(data_sources)
    |> Enum.map(&problem(&1, "The data source is unavailable. No cloaks serving this data source are online"))
    offline
  end

  defp problem(resource, description), do:
    %{resource: resource, description: description}

  defp offline_datasources(data_sources), do:
    Enum.filter(data_sources, fn(data_source) -> Cloak.channel_pids(data_source.name) == [] end)
end
