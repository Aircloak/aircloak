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
    assert :ok == Customer.mark_air_online(customer, "air1", [])
    assert :online == air_data(customer, "air1").status
  end

  test "storing air status with online cloaks" do
    customer = create_customer()
    assert :ok == Customer.mark_air_online(customer, "air1", online_cloaks())
    assert :online == air_data(customer, "air1").status
    assert :online == cloak_data(customer, "air1", "cloak1").status
    assert :online == cloak_data(customer, "air1", "cloak2").status
  end

  test "updating air status" do
    customer = create_customer()
    Customer.mark_air_online(customer, "air1", [])
    Customer.mark_air_offline(customer, "air1")
    assert :offline == air_data(customer, "air1").status
  end

  test "updating air status to offline will set cloaks to offline as well" do
    customer = create_customer()
    Customer.mark_air_online(customer, "air1", online_cloaks())
    Customer.mark_air_offline(customer, "air1")
    assert :offline == cloak_data(customer, "air1", "cloak1").status
    assert :offline == cloak_data(customer, "air1", "cloak2").status
  end

  test "resetting air statuses" do
    customer = create_customer()
    Customer.mark_air_online(customer, "air1", online_cloaks())
    Customer.mark_air_online(customer, "air2", [])
    Customer.reset_air_statuses()
    assert :offline == air_data(customer, "air1").status
    assert :offline == air_data(customer, "air2").status
    assert :offline == cloak_data(customer, "air1", "cloak1").status
    assert :offline == cloak_data(customer, "air1", "cloak2").status
  end

  test "storing cloak status" do
    customer = create_customer()
    assert :ok == Customer.mark_air_online(customer, "air1", [])
    assert :ok == Customer.update_cloak(customer, "air1", "cloak1", status: :online)
    assert :online == cloak_data(customer, "air1", "cloak1").status
  end

  test "updating cloak status" do
    customer = create_customer()
    assert :ok == Customer.mark_air_online(customer, "air1", online_cloaks())
    Customer.update_cloak(customer, "air1", "cloak1", status: :offline)
    assert :offline == cloak_data(customer, "air1", "cloak1").status
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

  defp air_data(customer, air_name), do:
    Enum.find(Customer.airs(), &(&1.name == air_name && &1.customer.id == customer.id))

  defp cloak_data(customer, air_name, cloak_name), do:
    Enum.find(air_data(customer, air_name).cloaks, &(&1.name == cloak_name))

  defp online_cloaks(), do:
    [%{name: "cloak1", data_sources: 1}, %{name: "cloak2", data_sources: 2}]
end
