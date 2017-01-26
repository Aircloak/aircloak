defmodule Central.Service.CustomerTest do
  use Central.ModelCase, async: true

  alias Central.Schemas
  alias Central.Service.Customer

  test "returns customers - none when none exist" do
    assert Customer.all() == []
  end

  test "returns customers - all in the db" do
    create_customer("customer 1")
    create_customer("customer 2")
    assert Enum.map(Customer.all(), &(&1.name)) === ["customer 1", "customer 2"]
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

  test "can delete existing customers" do
    assert {:ok, customer} = Customer.create(%{name: "customer"})
    assert :ok == Customer.delete(customer)
  end

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

  test "storing air status" do
    customer = create_customer()
    assert :ok == Customer.update_air_status(customer, "air1", :online)
    assert 1 == Repo.one!(from(a in "airs", where: a.name == "air1", select: a.status))
  end

  test "updating air status" do
    customer = create_customer()
    assert :ok == Customer.update_air_status(customer, "air1", :online)
    assert :ok == Customer.update_air_status(customer, "air1", :offline)
    assert 0 == Repo.one!(from(a in "airs", where: a.name == "air1", select: a.status))
  end

  test "records query executions" do
    metrics = %{"user_count" => 10}
    features = %{"some" => "feature"}
    aux = %{"other" => "data"}
    params = %{
      metrics: metrics,
      features: features,
      aux: aux,
    }
    customer = create_customer()
    assert :ok == Customer.record_query(customer, params)
    customer = Repo.preload(customer, :queries)
    [query] = customer.queries
    assert query.metrics == metrics
    assert query.features == features
    assert query.aux == aux
  end

  defp create_customer(name \\ "default customer") do
    assert {:ok, customer} = Repo.insert(Schemas.Customer.changeset(%Schemas.Customer{}, %{name: name}))
    customer
  end
end
