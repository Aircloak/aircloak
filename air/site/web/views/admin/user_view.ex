defmodule Air.Admin.UserView do
  @moduledoc false
  use Air.Web, :view
  # bug in the current Phoenix
  @dialyzer :no_match

  alias Air.{Repo, Group}

  defp select_org_values(conn) do
    organisations = conn.assigns[:organisations]
    Enum.reduce(organisations, [], fn(org, acc) ->
          [{org.name, org.id} | acc]
        end)
  end

  defp select_org_options(changeset, options) do
    options = [{:prompt, "Please select an organisation"} | options]
    case changeset.data.organisation_id do
      nil -> options
      id -> [{:selected, id} | options]
    end
  end

  defp organisation_name(user) do
    case user.organisation do
      nil -> "Not assigned an organisation"
      org -> org.name
    end
  end

  defp role(user),
    do: Air.User.role_description(user)

  defp possible_roles do
    Air.User.all_roles()
    |> Stream.filter(&(not match?({_id, {:admin, _}}, &1)))
    |> Enum.map(fn({id, {_key, desc}}) -> {desc, id} end)
  end

  defp group_names([]), do: "No groups"
  defp group_names(groups) when length(groups) <= 3 do
    join_groups(groups)
  end
  defp group_names(groups) do
    {first_groups, rest} = Enum.split(groups, 2)
    join_groups(first_groups ++ [%{name: "#{length(rest)} other groups"}])
  end

  # Goddamn Oxford comman :|
  defp join_groups(groups) when length(groups) < 3 do
    groups
    |> Enum.map(&(&1.name))
    |> Enum.join(", ")
  end
  defp join_groups(groups) do
    {most, last} = groups
    |> Enum.map(&(&1.name))
    |> Enum.split(length(groups) - 1)
    most = Enum.join(most, ", ")
    "#{most}, and #{last}"
  end

  defp available_groups() do
    Repo.all(Group)
  end
end
