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

  defp group_names(user) do
    {first_groups, rest} = Enum.split(user.groups, 3)
    names = case first_groups do
      [] -> "No groups"
      _ ->
        first_groups
        |> Enum.map(&(&1.name))
        |> Enum.join(", ")
    end
    case length(rest) do
      0 -> names
      n -> "#{names} and #{n} others"
    end
  end

  defp available_groups() do
    Repo.all(Group)
  end
end
