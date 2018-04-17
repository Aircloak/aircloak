defmodule AirWeb.Plug.ValidatePrivacyPolicy do
  @moduledoc """
  A set of plugs for validations relating to privacy policies.
  """

  alias AirWeb.Plug.ValidatePrivacyPolicy

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
        cond do
          Air.Service.PrivacyPolicy.exists?() ->
            conn

          admin?(conn) and ValidatePrivacyPolicy.in_privacy_policy_section?(conn) ->
            conn

          admin?(conn) ->
            conn
            |> Phoenix.Controller.redirect(to: AirWeb.Router.Helpers.admin_privacy_policy_path(conn, :new))
            |> Plug.Conn.halt()

          _otherwise = true ->
            ValidatePrivacyPolicy.halt_with_policy_notification(conn, :missing)
        end
      end

      # -------------------------------------------------------------------
      # Internal functions
      # -------------------------------------------------------------------

      defp admin?(conn), do: Air.Schemas.User.admin?(conn.assigns.current_user)
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
          ValidatePrivacyPolicy.halt_with_policy_notification(conn, :missing)
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
      cond do
        policy_status_ok?(conn) ->
          conn

        ValidatePrivacyPolicy.in_privacy_policy_section?(conn) ->
          conn

        _otherwise = true ->
          {:ok, privacy_policy} = Air.Service.PrivacyPolicy.get()
          ValidatePrivacyPolicy.halt_with_policy_notification(conn, :review, privacy_policy: privacy_policy)
      end
    end

    # -------------------------------------------------------------------
    # Internal functions
    # -------------------------------------------------------------------

    defp policy_status_ok?(conn), do: Air.Service.User.privacy_policy_status(conn.assigns.current_user) == :ok
  end

  @doc "Renders a privacy policy view and halts the plug chain"
  @spec halt_with_policy_notification(Plug.Conn.t(), :review | :missing) :: Plug.Conn.t()
  def halt_with_policy_notification(conn, notification, assigns \\ []) do
    conn
    |> Plug.Conn.put_status(Plug.Conn.Status.code(:precondition_failed))
    |> Phoenix.Controller.render(AirWeb.PrivacyPolicyView, notification, assigns)
    |> Plug.Conn.halt()
  end

  @doc "Returns true if the requested path is in the privacy policy section of the admin interface"
  @spec in_privacy_policy_section?(Plug.Conn.t()) :: boolean
  def in_privacy_policy_section?(conn) do
    admin_path = AirWeb.Router.Helpers.admin_privacy_policy_path(conn, :index)
    customer_path = AirWeb.Router.Helpers.privacy_policy_path(conn, :index)
    String.starts_with?(conn.request_path, admin_path) or String.starts_with?(conn.request_path, customer_path)
  end
end
