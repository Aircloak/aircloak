defmodule AirWeb.Admin.AccountingController do
  @moduledoc false
  use Air.Web, :admin_controller

  alias Air.Schemas.Query


  # -------------------------------------------------------------------
  # AirWeb.VerifyPermissions callback
  # -------------------------------------------------------------------

  def permissions do
    %{
      admin: [:show]
    }
  end


  # -------------------------------------------------------------------
  # Actions
  # -------------------------------------------------------------------

  def show(conn, _params) do
    au_per_month = from query in Query,
      group_by: [
        fragment("year"),
        fragment("month"),
      ],
      order_by: [desc: fragment("year"), desc: fragment("month")],
      select: %{
        year: fragment("extract(year from updated_at) as year"),
        month: fragment("extract(month from updated_at) as month"),
        sum: sum(query.users_count),
      }
    by_month = Repo.all(au_per_month)

    au_by_user = from query in Query,
      inner_join: user in assoc(query, :user),
      group_by: user.id,
      order_by: user.name,
      select: %{
        name: user.name,
        sum: sum(query.users_count)
      }
    by_user = Repo.all(au_by_user)

    render(conn, "show.html", by_month: by_month, by_user: by_user)
  end
end
