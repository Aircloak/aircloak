defmodule AirWeb.Plug.ValidatePrivacyPolicy do
  @moduledoc """
  A set of plugs for validations relating to privacy policies.
  """

  defmodule Existence do
    @moduledoc """
    If a privacy policy exists nothing is done.
    If one doesn't exist then:
    - admin web users are redirected to a page where they can create a privacy policy
    - all other users are shown a message indicating that a privacy policy has to be created
    """

    defmodule Browser do
      @moduledoc false
      @behaviour Plug

      @impl Plug
      def init(opts), do: opts

      @impl Plug
      def call(conn, _opts) do
        if Air.Service.PrivacyPolicy.exists?() do
          conn
        else
          if Air.Schemas.User.admin?(conn.assigns.current_user) do
            AirWeb.Plug.ValidatePrivacyPolicy.unless_in_privacy_policy_section(conn, fn ->
              redirect_to_policy_creation(conn)
            end)
          else
            AirWeb.Plug.ValidatePrivacyPolicy.halt_with_policy_notification(conn, :missing)
          end
        end
      end

      # -------------------------------------------------------------------
      # Internal functions
      # -------------------------------------------------------------------

      defp redirect_to_policy_creation(conn) do
        conn
        |> Phoenix.Controller.redirect(to: AirWeb.Router.Helpers.admin_privacy_policy_path(conn, :new))
        |> Plug.Conn.halt()
      end
    end

    defmodule API do
      @moduledoc false
      @behaviour Plug

      @impl Plug
      def init(opts), do: opts

      @impl Plug
      def call(conn, _opts) do
        if Air.Service.PrivacyPolicy.exists?() do
          conn
        else
          AirWeb.Plug.ValidatePrivacyPolicy.halt_with_policy_notification(conn, :missing)
        end
      end
    end
  end

  defmodule Acceptance do
    @moduledoc """
    Assumes that the Existence plug has already been run, and that a privacy policy
    therefore is in place.

    Redirects all web users to the privacy policy page if they have not yet accepted the policy.
    API users have their requests halted.
    """
    @behaviour Plug

    @impl Plug
    def init(opts), do: opts

    @impl Plug
    def call(conn, _opts) do
      if Air.Service.User.privacy_policy_status(conn.assigns.current_user) == :ok do
        conn
      else
        AirWeb.Plug.ValidatePrivacyPolicy.unless_in_privacy_policy_section(conn, fn ->
          AirWeb.Plugs.Utils.if_in_section(conn, AirWeb.Router.Helpers.privacy_policy_path(conn, :index), fn ->
            {:ok, privacy_policy} = Air.Service.PrivacyPolicy.get()

            AirWeb.Plug.ValidatePrivacyPolicy.halt_with_policy_notification(
              conn,
              :review,
              privacy_policy: privacy_policy
            )
          end)
        end)
      end
    end
  end

  @doc "Runs the provided callback unless the request is for a resource in the admin privacy policy section"
  @spec unless_in_privacy_policy_section(Plug.Conn.t(), (() -> Plug.Conn.t())) :: Plug.Conn.t()
  def unless_in_privacy_policy_section(conn, callback) do
    admin_privacy_policy_path_prefix = AirWeb.Router.Helpers.admin_privacy_policy_path(conn, :index)
    AirWeb.Plugs.Utils.if_in_section(conn, admin_privacy_policy_path_prefix, callback)
  end

  @doc "Renders a privacy policy view and halts the plug chain"
  @spec halt_with_policy_notification(Plug.Conn.t(), :review | :missing) :: Plug.Conn.t()
  def halt_with_policy_notification(conn, notification, assigns \\ []) do
    conn
    |> Plug.Conn.put_status(Plug.Conn.Status.code(:precondition_failed))
    |> Phoenix.Controller.render(AirWeb.PrivacyPolicyView, notification, assigns)
    |> Plug.Conn.halt()
  end
end
