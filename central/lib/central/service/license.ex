defmodule Central.Service.License do
  @moduledoc "Service module for working with licenses."

  alias Central.{Repo, Service, Schemas.License, Schemas.Customer}
  alias Ecto.Changeset
  import Ecto.Query


  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @doc "Creates a new licenses with the given params."
  @spec create(Customer.t, map) :: {:ok, License.t} | {:error, Changeset.t}
  def create(customer, params), do:
    customer
    |> Ecto.build_assoc(:licenses)
    |> Changeset.change(revoked: false)
    |> License.changeset(params)
    |> Repo.insert()

  @doc "Updates the given licenses with the given params."
  @spec update(License.t, map) :: {:ok, License.t} | {:error, Changeset.t}
  def update(license, params), do:
    license
    |> License.changeset(params)
    |> Repo.update()

  @doc "Returns all licenses for the given customer."
  @spec for_customer(Customer.t) :: [License.t]
  def for_customer(customer), do:
    License
    |> for_customer_id(customer.id)
    |> Repo.all()

  @doc "Returns a changeset representing no changes to the given license."
  @spec empty_changeset(License.t) :: Changeset.t
  def empty_changeset(license \\ %License{}), do: License.changeset(license)

  @doc "Returns an encrypted representation of this license."
  @spec export(License.t) :: String.t
  def export(license), do:
    %{
      id: license.id,
      customer_id: license.customer_id,
      expires_at: expires_at(license) |> Timex.format!("{ISO:Basic:Z}")
    }
    |> Poison.encode!()
    |> encrypt!()

  @doc "Returns the license with the given id for the given customer."
  @spec get(Customer.t, any) :: {:ok, License.t} | :not_found
  def get(customer, id) do
    License
    |> for_customer_id(customer.id)
    |> Repo.get(id)
    |> case do
      nil -> :not_found
      license -> {:ok, license}
    end
  end

  @doc """
  Returns the time when the given license expires. Note that for auto_renew licenses this is merely the time
  before which it will have to be renewed.
  """
  @spec expires_at(License.t) :: Timex.Types.valid_datetime() | Timex.AmbiguousDateTime.t() | {:error, term()}
  def expires_at(license), do:
    license |> base_time() |> Timex.shift(days: license.length_in_days)

  @doc "Revokes the given license. Revoked licenses keep all their attributes but are treated as not-auto-renew."
  @spec revoke(License.t) :: {:ok, License.t} | {:error, Changeset.t}
  def revoke(license), do: __MODULE__.update(license, %{revoked: true})

  @doc "Restores the given revoked license."
  @spec restore(License.t) :: {:ok, License.t} | {:error, Changeset.t}
  def restore(license), do: __MODULE__.update(license, %{revoked: false})

  @doc "Returns the license object matching the given license text."
  @spec decrypt(String.t) :: {:ok, License.t} | {:error, :invalid_license}
  def decrypt(text) do
    with \
         {:ok, %{customer_id: customer_id, license_id: license_id}} <- Aircloak.License.decrypt(public_key(), text),
         {:ok, customer} <- Service.Customer.get(customer_id),
         {:ok, license} <- get(customer, license_id)
    do
      {:ok, license}
    else
      _ -> {:error, :invalid_license}
    end
  end

  @doc "Returns a renewed version of the given license text."
  @spec renew(String.t) :: {:ok, String.t} | {:error, :invalid_license}
  def renew(text) do
    with {:ok, license} <- decrypt(text) do
      {:ok, export(license)}
    end
  end


  # -------------------------------------------------------------------
  # Scopes
  # -------------------------------------------------------------------

  defp for_customer_id(query, customer_id), do:
    where(query, [q], q.customer_id == ^customer_id)


  # -------------------------------------------------------------------
  # Exporting
  # -------------------------------------------------------------------

  defp base_time(%{revoked: true, inserted_at: inserted_at}), do: inserted_at
  defp base_time(%{auto_renew: true}), do: Timex.now()
  defp base_time(%{inserted_at: inserted_at}), do: inserted_at

  defp encrypt!(text) do
    {:ok, encrypted} = ExPublicKey.encrypt_private(text, private_key())
    encrypted
  end

  defp private_key(), do:
    Agent.get(__MODULE__, fn %{private_key: private_key} -> private_key end)

  defp public_key(), do:
    Agent.get(__MODULE__, fn %{public_key: public_key} -> public_key end)

  # -------------------------------------------------------------------
  # Supervision tree
  # -------------------------------------------------------------------

  @doc false
  def child_spec(_arg), do:
    Aircloak.ChildSpec.agent(&load_keys/0, name: __MODULE__)

  defp load_keys() do
    {:ok, public_key} = ExPublicKey.load(path(:public_key))
    {:ok, private_key} = ExPublicKey.load(path(:private_key))

    %{
      private_key: private_key,
      public_key: public_key,
    }
  end

  defp path(key) do
    root_path = Application.app_dir(:central)
    file_name = Application.get_env(:central, :license) |> Keyword.fetch!(key)
    Path.join([root_path, file_name])
  end
end
