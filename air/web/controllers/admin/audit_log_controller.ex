defmodule Air.Admin.AuditLogController do
  @moduledoc false
  use Air.Web, :admin_controller

  alias Air.Service.{AuditLog, User, DataSource}


  # -------------------------------------------------------------------
  # Air.VerifyPermissions callback
  # -------------------------------------------------------------------

  def permissions do
    %{
      admin: [:index]
    }
  end


  # -------------------------------------------------------------------
  # Actions
  # -------------------------------------------------------------------

  def index(conn, params) do
    service_params = %{
      page: params["page"] || 1,
      users: params["users"] || [],
      events: params["events"] || [],
      data_sources: params["data_sources"] || [],
    }
    render(conn, "index.html",
      audit_logs: AuditLog.for(service_params),
      full_width: true,
      users: users(service_params),
      event_types: event_types(service_params),
      data_sources: data_sources(service_params),
    )
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp event_types(params) do
    already_selected_event_types = params[:events]
    combined_event_types = Air.Service.AuditLog.event_types(params) ++
      already_selected_event_types
    Enum.uniq(combined_event_types)
  end

  defp users(params) do
    already_selected_users = params[:users]
    filterable_users = AuditLog.users(params)
    no_longer_available_users = already_selected_users -- (filterable_users |> Enum.map(&(&1.email)))
      |> User.by_emails()
      |> Enum.map(&(%{name: &1.name, email: &1.email}))
    filterable_users ++ no_longer_available_users
  end

  defp data_sources(params) do
    already_selected_data_sources = params[:data_sources] |> Enum.map(&String.to_integer/1)
    filterable_data_sources = AuditLog.data_sources(params)
    no_longer_available_data_sources = already_selected_data_sources --
      (filterable_data_sources |> Enum.map(&(&1.id)))
      |> DataSource.by_ids()
      |> Enum.map(&(%{name: &1.name, id: &1.id}))
    filterable_data_sources ++ no_longer_available_data_sources
  end
end
