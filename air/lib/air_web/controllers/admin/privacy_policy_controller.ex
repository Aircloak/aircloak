defmodule AirWeb.Admin.PrivacyPolicyController do
  @moduledoc false
  use Air.Web, :admin_controller

  alias Air.Service.PrivacyPolicy

  # -------------------------------------------------------------------
  # Actions
  # -------------------------------------------------------------------

  def index(conn, _params) do
    if PrivacyPolicy.exists?() do
      {:ok, current_privacy_policy} = PrivacyPolicy.get()

      render(
        conn,
        "index.html",
        privacy_policies: PrivacyPolicy.all(),
        current_privacy_policy: current_privacy_policy
      )
    else
      redirect(conn, to: admin_privacy_policy_path(conn, :new))
    end
  end

  def new(conn, _params) do
    privacy_policy =
      case PrivacyPolicy.get() do
        {:ok, privacy_policy} -> privacy_policy
        _ -> nil
      end

    render(conn, "new.html", privacy_policy: privacy_policy)
  end

  def show(conn, params) do
    id = params["id"]

    case PrivacyPolicy.get_by_id(id) do
      {:error, :not_found} ->
        conn
        |> put_flash(:error, "Privacy policy revision not found.")
        |> redirect(to: admin_privacy_policy_path(conn, :index))

      {:ok, policy} ->
        render(conn, "show.html", privacy_policy: policy)
    end
  end

  def edit(conn, _params) do
    render(conn, "edit.html")
  end

  def create(conn, params) do
    content = params["content"]

    changes =
      case params["changes"] do
        "" -> nil
        changes -> changes
      end

    PrivacyPolicy.set(content, changes)
    redirect(conn, to: admin_privacy_policy_path(conn, :index))
  end
end
