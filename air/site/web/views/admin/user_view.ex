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

  defp group_names(groups) do
    groups
    |> Enum.map(&(&1.name))
    |> Enum.map(&shorten_name/1)
    |> format_names()
  end

  def format_names([]), do: "No groups"
  def format_names([group1, group2]), do: "#{group1}, #{group2}"
  def format_names([group1, group2, group3]), do: "#{group1}, #{group2}, and #{group3}"
  def format_names(groups) do
    {first_groups, rest} = Enum.split(groups, 2)
    first_groups ++ ["and #{length(rest)} other groups"]
    |> Enum.join(", ")
  end

  @max_length 10

  def shorten_name(name) do
    if String.length(name) > @max_length do
      {first, _} = String.split_at(name, 4)
      first = String.trim(first)
      {_, last} = String.split_at(name, String.length(name) - 4)
      last = String.trim(last)
      "#{first}..#{last}"
    else
      name
    end
  end

  defp available_groups() do
    Repo.all(Group)
  end
end
