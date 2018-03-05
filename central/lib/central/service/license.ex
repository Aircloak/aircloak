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
    |> where([q], q.customer_id == ^customer.id)
    |> Repo.all()

  def empty_changeset(), do: License.changeset(%License{})
end
