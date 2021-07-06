defmodule Air.Service.Warnings do
  @moduledoc "A service providing warnings about problems in the system"

  @type severity_class :: :high | :medium | :low

  @type resource :: Schemas.DataSource.t() | :privacy_policy

  @type problem :: %{
          resource: resource,
          description: String.t(),
          severity: severity_class
        }

  alias Air.Service.{DataSource, Cloak}
  alias Air.{Schemas, Repo}

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc """
  Returns a list of problems encountered in the system.
  Problem range from a data source being offline, to it not being queryable by any users.
  """
  @spec problems() :: [problem]
  def problems(),
    do:
      data_source_problems(DataSource.all())
      |> order_problems()

  @doc """
  Returns a list of a problem for a particular resource.
  The structure of the warnings is the same as what is returned by problems/0
  """
  @spec problems_for_resource(resource) :: [problem]
  def problems_for_resource(%Schemas.DataSource{} = data_source),
    do: data_source_problems([data_source]) |> order_problems()

  @doc "Given a set of problems, returns the highest severity class of any of the problems"
  @spec highest_severity_class([problem]) :: severity_class
  def highest_severity_class(problems),
    do:
      problems
      |> Enum.map(& &1.severity)
      |> Enum.reduce(:low, &if(severity_to_number(&1) < severity_to_number(&2), do: &1, else: &2))

  # -------------------------------------------------------------------
  # Ordering
  # -------------------------------------------------------------------

  defp order_problems(problems), do: Enum.sort_by(problems, &{severity_to_number(&1.severity), &1.resource})

  defp severity_to_number(:high), do: 1
  defp severity_to_number(:medium), do: 2
  defp severity_to_number(:low), do: 3

  # -------------------------------------------------------------------
  # Data Source problems
  # -------------------------------------------------------------------

  defp data_source_problems(data_sources) do
    data_sources = Repo.preload(data_sources, groups: :users)

    offline_datasources(data_sources, :high) ++
      failed_isolator(data_sources, :medium) ++
      failed_shadow_tables(data_sources, :medium) ++
      failed_bounds(data_sources, :medium) ++
      broken_datasources(data_sources, :medium) ++
      no_group(data_sources, :low) ++
      no_users(data_sources, :low)
  end

  defp offline_datasources(data_sources, severity),
    do:
      data_sources
      |> Enum.filter(fn data_source -> Cloak.channel_pids(data_source.name) == [] end)
      |> Enum.map(
        &problem(
          &1,
          "The data source is unavailable. No cloaks serving this data source are online",
          severity
        )
      )

  defp failed_isolator(data_sources, severity) do
    for data_source = %{isolated_failed: failed = [_ | _]} <- data_sources do
      columns = failed |> Enum.map(&"`#{&1}`") |> Aircloak.OxfordComma.join()

      message =
        "Cloak could not compute if columns #{columns} are isolating." <>
          " The columns will be treated as isolating unless manually classified." <>
          " See the Restrictions section of the user guide for more information."

      problem(data_source, message, severity)
    end
  end

  defp failed_shadow_tables(data_sources, severity) do
    for data_source = %{shadow_tables_failed: failed = [_ | _]} <- data_sources do
      columns = failed |> Enum.map(&"`#{&1}`") |> Aircloak.OxfordComma.join()

      message =
        "Cloak could not compute frequent values from columns #{columns}." <>
          " The columns will be treated as having no frequent values." <>
          " See the Restrictions section of the user guide for more information."

      problem(data_source, message, severity)
    end
  end

  defp failed_bounds(data_sources, severity) do
    for data_source = %{bounds_failed: failed = [_ | _]} <- data_sources do
      columns = failed |> Enum.map(&"`#{&1}`") |> Aircloak.OxfordComma.join()

      message =
        "Cloak could not compute bounds for columns #{columns}." <>
          " Thes columns will be treated as unbounded," <>
          " which might result in query emulation for some mathematical operations."

      problem(data_source, message, severity)
    end
  end

  defp broken_datasources(data_sources, severity),
    do:
      data_sources
      |> Enum.reject(&(&1.errors === "" or &1.errors === "[]"))
      |> Enum.flat_map(&unwrap_errors(&1, severity))

  defp unwrap_errors(data_source, severity),
    do:
      data_source.errors
      |> Jason.decode!()
      |> Enum.map(&problem(data_source, &1, severity))

  defp no_group(data_sources, severity),
    do:
      data_sources
      |> Enum.filter(&Enum.empty?(&1.groups))
      |> Enum.map(
        &problem(
          &1,
          "No groups have been given access to the data source. It cannot be queried",
          severity
        )
      )

  defp no_users(data_sources, severity),
    do:
      data_sources
      |> Enum.filter(fn data_source -> Enum.all?(data_source.groups, &Enum.empty?(&1.users)) end)
      |> Enum.map(&problem(&1, "No users have access to this data source", severity))

  # -------------------------------------------------------------------
  # Helpers
  # -------------------------------------------------------------------

  defp problem(resource, description, severity), do: %{resource: resource, description: description, severity: severity}
end
