defmodule AirWeb.Admin.PrivacyPolicyController do
  @moduledoc false
  use Air.Web, :admin_controller

  alias Air.Service.PrivacyPolicy

  # -------------------------------------------------------------------
  # AirWeb.VerifyPermissions callback
  # -------------------------------------------------------------------

  def permissions do
    %{
      admin: :all,
      user: [:invalid]
    }
  end

  # -------------------------------------------------------------------
  # Actions
  # -------------------------------------------------------------------

  def index(conn, _params) do
    if PrivacyPolicy.exists?() do
      render(conn, "index.html", policies: PrivacyPolicy.all())
    else
      redirect(conn, to: admin_privacy_policy_path(conn, :new))
    end
  end

  def new(conn, _params) do
    content =
      if PrivacyPolicy.exists?() do
        {:ok, policy} = PrivacyPolicy.get()
        policy.content
      else
        PrivacyPolicy.default_content()
      end

    render(conn, "new.html", content: content)
  end

  def show(conn, params) do
    id = params["id"]

    case PrivacyPolicy.get_by_id(id) do
      {:error, :not_found} ->
        conn
        |> put_flash(:error, "Privacy policy revision not found")
        |> redirect(to: admin_privacy_policy_path(conn, :index))

      {:ok, policy} ->
        render(conn, "show.html", policy: policy)
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
