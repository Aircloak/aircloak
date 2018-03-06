defmodule Central.Service.License do
  alias Central.{Repo, Schemas.License}
  import Ecto.Query

  def create(customer, params), do:
    customer
    |> Ecto.build_assoc(:licenses)
    |> License.changeset(params)
    |> Repo.insert()

  def for_customer(customer), do:
    License
    |> for_customer_id(customer.id)
    |> Repo.all()

  def empty_changeset(), do: License.changeset(%License{})

  def export(customer, id) do
    License
    |> for_customer_id(customer.id)
    |> Repo.get(id)
    |> case do
      nil -> :not_found
      license -> {:ok, format_export(license) |> Poison.encode!()}
    end
  end

  defp format_export(license), do:
    %{id: license.id, customer_id: license.customer_id, expires_at: expires_at(license)}

  defp for_customer_id(query, customer_id), do:
    where(query, [q], q.customer_id == ^customer_id)

  defp expires_at(license), do:
    Timex.now() |> Timex.shift(days: license.length_in_days) |> Timex.format!("{ISO:Basic}")


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
