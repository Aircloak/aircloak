defmodule Central.Service.Customer do
  @moduledoc "Service module for working with users"

  require Logger
  require Aircloak.DeployConfig

  alias Ecto.Changeset
  alias Central.Repo
  alias Central.Schemas.{Customer, License}

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
end
