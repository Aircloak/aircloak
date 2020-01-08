defmodule Central.Service.CustomerTest do
  use Central.ModelCase, async: true

  alias Central.Schemas
  alias Central.Service.{Customer, License}

  test "returns customers - none when none exist" do
    assert Customer.all() == []
  end

  test "returns customers - all in the db" do
    create_customer("customer 1")
    create_customer("customer 2")
    assert Enum.map(Customer.all(), & &1.name) === ["customer 1", "customer 2"]
  end

  test "can create customers" do
    assert {:ok, _} = Customer.create(%{name: "customer"})
  end

  test "fails on creating with repeated name" do
    assert {:ok, _} = Customer.create(%{name: "customer"})
    assert {:error, _} = Customer.create(%{name: "customer"})
  end

  test "can get a customer when it exists" do
    assert {:ok, customer} = Customer.create(%{name: "customer"})
    {:ok, loaded_customer} = Customer.get(customer.id)
    assert loaded_customer.name == customer.name
  end

  test "fails when loaded customer doesn't exist" do
    assert {:error, :not_found} == Customer.get(1)
  end

  test "can update a customer" do
    assert {:ok, customer} = Customer.create(%{name: "customer"})
    assert {:ok, _} = Customer.update(customer, %{name: "new name"})
    {:ok, loaded_customer} = Customer.get(customer.id)
    assert loaded_customer.name !== customer.name
  end

  describe "delete" do
    test "can delete existing customers" do
      assert {:ok, customer} = Customer.create(%{name: "customer"})
      assert :ok == Customer.delete(customer)
    end

    test "deletes the customer's licenses" do
      {:ok, customer} = Customer.create(%{name: "customer"})
      {:ok, license} = License.create(customer, %{name: "some license", length_in_days: 10, auto_renew: true})

      Customer.delete(customer)

      refute Repo.get(Schemas.License, license.id)
    end
  end

  describe "token management" do
    test "creates tokens for customer" do
      assert {:ok, _} = Customer.generate_token(create_customer())
    end

    test "can load customers from token" do
      customer = create_customer()
      {:ok, token} = Customer.generate_token(customer)
      {:ok, loaded_customer} = Customer.from_token(token)
      assert loaded_customer.id === customer.id
    end

    test "returns an invalid token error for missing customers" do
      customer = create_customer()
      {:ok, token} = Customer.generate_token(customer)
      Customer.delete(customer)
      assert {:error, :invalid_token} = Customer.from_token(token)
    end

    test "returns an invalid token bogus tokens" do
      assert {:error, :invalid_token} = Customer.from_token("bogus token")
    end
  end

  defp create_customer(name \\ "default customer") do
    assert {:ok, customer} = Repo.insert(Schemas.Customer.changeset(%Schemas.Customer{}, %{name: name}))

    customer
  end
end
