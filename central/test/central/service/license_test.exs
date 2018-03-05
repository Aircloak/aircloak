defmodule Central.Service.License.Test do
  use Central.ModelCase, async: true

  alias Central.Schemas
  alias Central.Service.License

  setup [:setup_customer]

  test "returns no licenses for a customer when none exist", %{customer: customer} do
    assert [] = License.for_customer(customer)
  end

  test "creates a license", %{customer: customer} do
    {:ok, _} = License.create(customer, %{name: "some license", length_in_days: 10, auto_renew: true})
    assert [%{name: "some license"}] = License.for_customer(customer)
  end

  test "returns no licenses for a customer when other customers have licenses", %{customer: customer} do
    {:ok, _} = License.create(customer, %{name: "some license", length_in_days: 10, auto_renew: true})
    assert [] = License.for_customer(create_customer("Some other guy"))
  end

  test "creating an invalid license", %{customer: customer} do
    assert {:error, _} = License.create(customer, %{name: ""})
  end

  defp setup_customer(_) do
    {:ok, %{customer: create_customer()}}
  end

  defp create_customer(name \\ "Some name") do
    {:ok, customer} = Repo.insert(Schemas.Customer.changeset(%Schemas.Customer{}, %{name: name}))
    customer
  end
end
