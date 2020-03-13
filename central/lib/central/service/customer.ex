defmodule Central.Service.Customer do
  @moduledoc "Service module for working with users"

  require Logger
  require Aircloak.DeployConfig

  alias Central.Repo
  alias Central.Schemas.{Customer, License}
  alias Ecto.Changeset

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Returns all registered customers"
  @spec all() :: [Customer.t()]
  def all(), do: Repo.all(Customer)

  @doc "Creates a customer"
  @spec create(Map.t()) :: {:ok, Customer.t()} | {:error, Changeset.t()}
  def create(params) do
    changeset = Customer.new_customer_changeset(Customer.empty_changeset(), params)
    Repo.insert(changeset)
  end

  @doc "Updates a customer"
  @spec update(Customer.t(), Map.t()) :: {:ok, Customer.t()} | {:error, Changeset.t()}
  def update(customer, params) do
    changeset = Customer.changeset(customer, params)
    Repo.update(changeset)
  end

  @doc "Removes a customer"
  @spec delete(Customer.t()) :: :ok | :error
  def delete(customer) do
    case Repo.delete(customer) do
      {:ok, _} -> :ok
      {:error, _changeset} -> :error
    end
  end

  @doc "Returns a customer by id"
  @spec get(non_neg_integer) :: {:ok, Customer.t()} | {:error, :not_found}
  def get(id) do
    case Repo.get(Customer, id) do
      nil -> {:error, :not_found}
      customer -> {:ok, customer}
    end
  end

  @doc """
  Generates a cryptographically signed token that is tied to a customer, and can
  be used to identify a customer through APIs.
  """
  @spec generate_token(Customer.t()) :: {:ok, String.t()}
  def generate_token(customer) do
    {:ok, Phoenix.Token.sign(secret_key_base(), customer_token_salt(), customer.id)}
  end

  @doc """
  Returns the customer associated with a token.
  Returns an error either if the customer doesn't exist, and likewise
  if the token is invalid.
  """
  @spec from_token(String.t()) :: {:ok, Customer.t()} | {:error, :invalid_token}
  def from_token(token) do
    Phoenix.Token.verify(
      secret_key_base(),
      customer_token_salt(),
      token,
      max_age: almost_infinity()
    )
    |> case do
      {:ok, customer_id} ->
        case get(customer_id) do
          {:error, :not_found} -> {:error, :invalid_token}
          other -> other
        end

      _ ->
        {:error, :invalid_token}
    end
  end

  @doc "Returns the customer associated with the given license."
  @spec from_license(License.t()) :: {:ok, Customer.t()} | {:error, :invalid_license}
  def from_license(license) do
    Customer
    |> Repo.get(license.customer_id)
    |> case do
      nil -> :error
      customer -> {:ok, customer}
    end
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  # Phoenix warns if we're not validating the token age, so we need to pass some integer value.
  # Therefore, we're simulating infinity by using a ridiculously large value (10,000 years).
  defp almost_infinity(), do: 60 * 60 * 24 * 365 * 10_000

  defp customer_token_salt() do
    Central.site_setting("customer_token_salt")
  end

  defp secret_key_base() do
    Central.site_setting("endpoint_key_base")
  end
end
