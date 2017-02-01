defmodule Central.Service.Customer do
  @moduledoc "Service module for working with users"

  require Logger
  require Aircloak.DeployConfig

  alias Ecto.Changeset
  alias Central.Repo
  alias Central.Schemas.{Air, Cloak, Customer, Query, OnlineStatus}
  alias Central.Service.ElasticSearch

  import Ecto.Query, only: [from: 2]


  #-----------------------------------------------------------------------------------------------------------
  # API functions
  #-----------------------------------------------------------------------------------------------------------

  @doc "Returns all registered customers"
  @spec all() :: [Customer.t]
  def all() do
    Repo.all(Customer)
  end

  @doc "Creates a customer"
  @spec create(Map.t) :: {:ok, Customer.t} | {:error, Changeset.t}
  def create(params) do
    changeset = Customer.new_customer_changeset(Customer.empty_changeset(), params)
    Repo.insert(changeset)
  end

  @doc "Updates a customer"
  @spec update(Customer.t, Map.t) :: {:ok, Customer.t} | {:error, Changeset.t}
  def update(customer, params) do
    changeset = Customer.changeset(customer, params)
    Repo.update(changeset)
  end

  @doc "Removes a customer"
  @spec delete(Customer.t) :: :ok | :error
  def delete(customer) do
    case Repo.delete(customer) do
      {:ok, _} -> :ok
      {:error, _changeset} -> :error
    end
  end

  @doc "Returns a cusstomer by id"
  @spec get(non_neg_integer) :: {:ok, Customer.t} | {:error, :not_found}
  def get(id) do
    case Repo.get(Customer, id) do
      nil -> {:error, :not_found}
      user -> {:ok, user}
    end
  end

  @doc """
  Generates a cryptographically signed token that is tied to a customer, and can
  be used to identify a customer through APIs.
  """
  @spec generate_token(Customer.t) :: {:ok, String.t}
  # The dialyzer suppression is due to a bug in Phoenix whereby :milli_seconds
  # is used instead of :milliseconds. This has been fixed in more recent
  # versions of Phoenix: https://github.com/phoenixframework/phoenix/pull/1986
  @dialyzer :no_return
  def generate_token(customer) do
    {:ok, Phoenix.Token.sign(secret_key_base(), customer_token_salt(), customer.id)}
  end

  @doc """
  Returns the customer associated with a token.
  Returns an error either if the customer doesn't exist, and likewise
  if the token is invalid.
  """
  @spec from_token(String.t) :: {:ok, Customer.t} | {:error, :invalid_token}
  def from_token(token) do
    case Phoenix.Token.verify(secret_key_base(), customer_token_salt(), token) do
      {:ok, customer_id} ->
        case get(customer_id) do
          {:error, :not_found} -> {:error, :invalid_token}
          other -> other
        end
      _ -> {:error, :invalid_token}
    end
  end

  @doc "Records a query execution associated with a customer"
  @spec record_query(Customer.t, Map.t) :: :ok | :error
  def record_query(customer, params) do
    ElasticSearch.record_query(customer, params)
    changeset = customer
      |> Ecto.build_assoc(:queries)
      |> Query.changeset(params)
    case Repo.insert(changeset) do
      {:ok, _} -> :ok
      {:error, changeset} ->
        Logger.error("Failed to insert query for customer #{customer.name} (#{customer.id}): " <>
          inspect(changeset))
        :error
    end
  end

  @doc "Marks air and associated cloaks as online."
  @spec mark_air_online(Customer.t, String.t, [%{name: String.t, data_sources: non_neg_integer}]) :: :ok
  def mark_air_online(customer, air_name, online_cloaks) do
    {:ok, air} = Repo.transaction(fn ->
      air = update_air_status(customer, air_name, :online)
      Enum.each(online_cloaks, &update_cloak(customer, air_name, &1.name,
        status: :online, data_sources: &1.data_sources))
      air
    end)

    ElasticSearch.record_air_presence(air)

    :ok
  end

  @doc "Marks air and all known cloaks as offline."
  @spec mark_air_offline(Customer.t, String.t) :: :ok
  def mark_air_offline(customer, air_name) do
    {:ok, air} = Repo.transaction(fn ->
      air = update_air_status(customer, air_name, :offline)
      Repo.update_all(from(c in Cloak, where: c.air_id == ^air.id), set: [status: :offline])
      air
    end)

    ElasticSearch.record_air_presence(air)

    :ok
  end

  # Error in current Ecto: https://github.com/elixir-ecto/ecto/issues/1882
  @dialyzer {:no_opaque, reset_air_statuses: 0}
  @doc "Resets statuses of all known airs and associated cloaks to offline."
  @spec reset_air_statuses() :: :ok
  def reset_air_statuses() do
    {:ok, _} =
      Ecto.Multi.new()
      |> Ecto.Multi.update_all(:set_airs_offline, Air, set: [status: :offline])
      |> Ecto.Multi.update_all(:set_cloaks_offline, Cloak, set: [status: :offline])
      |> Repo.transaction()

    :ok
  end

  @doc "Returns the list of all known airs."
  @spec airs() :: [Air.t]
  def airs(), do:
    Repo.all(from Air, preload: [:customer, :cloaks])

  @doc "Updates the cloak status."
  @spec update_cloak(Customer.t, String.t, String.t, [status: OnlineStatus.t, data_sources: non_neg_integer]) :: :ok
  def update_cloak(customer, air_name, cloak_name, updates) do
    cloak = Map.merge(
      %Cloak{air: Repo.get_by!(Ecto.assoc(customer, :airs), name: air_name), name: cloak_name},
      updates |> Keyword.take([:status, :data_sources]) |> Enum.into(%{})
    )
    Repo.insert!(cloak, on_conflict: [set: updates], conflict_target: [:name, :air_id])

    :ok
  end

  @doc "Stores the uptime info sent from air."
  @spec store_uptime_info(Customer.t, String.t, NaiveDateTime.t, Map.t) :: :ok
  def store_uptime_info(customer, air_name, air_utc_time, data) do
    mtime = NaiveDateTime.utc_now()

    Repo.insert_all("usage_info", [[
      customer_id: customer.id,
      air_id: Repo.get_by!(Ecto.assoc(customer, :airs), name: air_name).id,
      air_utc_time: air_utc_time,
      data: data,
      inserted_at: mtime,
      updated_at: mtime,
    ]])

    :ok
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp customer_token_salt() do
    Central.site_setting("customer_token_salt")
  end

  defp secret_key_base() do
    Central.site_setting("endpoint_key_base")
  end

  defp update_air_status(customer, air_name, status), do:
    Repo.insert!(
      %Air{name: air_name, customer: customer, status: status},
      on_conflict: [set: [status: status]],
      conflict_target: [:name, :customer_id]
    )
end
