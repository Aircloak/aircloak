defmodule Air.UserView do
  use Air.Web, :view

  def select_org_values(conn) do
    organisations = conn.assigns[:organisations]
    Enum.reduce(organisations, [], fn(org, acc) ->
          [{org.name, org.id} | acc]
        end)
  end

  def select_org_options(changeset, options \\ []) do
    options = [{:prompt, "Please select an organisation"} | options]
    case changeset.model.organisation_id do
      nil -> options
      id -> [{:selected, id} | options]
    end
  end

  def organisation_name(user) do
    case user.organisation do
      nil -> "Not assigned an organisation"
      org -> org.name
    end
  end
end
