defmodule Central.Service.Customer do
  @moduledoc "Service module for working with users"

  require Logger

  alias Ecto.Changeset
  alias Central.Repo
  alias Central.Schemas.{Customer, Query}

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
  def generate_token(customer) do
    {:ok, Phoenix.Token.sign(Central.Endpoint, customer_token_salt(), customer.id)}
  end

  @doc """
  Returns the customer associated with a token.
  Returns an error either if the customer doesn't exist, and likewise
  if the token is invalid.
  """
  @spec from_token(String.t) :: {:ok, Customer.t} | {:error, :invalid_token}
  def from_token(token) do
    case Phoenix.Token.verify(Central.Endpoint, customer_token_salt(), token) do
      {:ok, customer_id} ->
        case get(customer_id) do
          {:error, :not_found} -> {:error, :invalid_token}
          other -> other
        end
      _ -> {:error, :invalid_token}
    end
  end

  @doc "Records a query execution associated with a customer"
  @spec record_query(Customer.t, Map.t, Map.t) :: :ok | :error
  def record_query(customer, metrics, features) do
    changeset = customer
      |> Ecto.build_assoc(:queries)
      |> Query.changeset(%{metrics: metrics, features: features})
    case Repo.insert(changeset) do
      {:ok, _} -> :ok
      {:error, changeset} ->
        Logger.error("Failed to insert query for customer #{customer.name} (#{customer.id}): " <>
          inspect(changeset))
        :error
    end
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp customer_token_salt do
    Application.get_env(:central, Central.Endpoint) |> Keyword.fetch!(:customer_token_salt)
  end
end
