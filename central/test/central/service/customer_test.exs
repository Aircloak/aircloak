defmodule Central.Service.CustomerTest do
  use Central.ModelCase, async: true

  alias Central.Schemas
  alias Central.Service.Customer

  @air_version "21.1.0"
  @cloak1_version "22.1.0"
  @cloak2_version "23.1.0"

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

  test "store_rpc!" do
    rpc = Customer.store_rpc!(create_customer(), "foo", "bar")
    assert Repo.get(Schemas.AirRPC, rpc.id) != nil
  end

  test "rpc_imported? returns false if customer doesn't match" do
    Customer.store_rpc!(create_customer("c1"), "foo", "bar")
    assert Customer.rpc_imported?(create_customer("c2"), "foo", "bar") == false
  end

  test "rpc_imported? returns false if air name doesn't match" do
    customer = create_customer()
    Customer.store_rpc!(customer, "air", "id")
    assert Customer.rpc_imported?(customer, "another_air", "id") == false
  end

  test "rpc_imported? returns false if id doesn't match" do
    customer = create_customer()
    Customer.store_rpc!(customer, "air", "id")
    assert Customer.rpc_imported?(customer, "air", "another_id") == false
  end

  test "rpc_imported? returns true if all fields match" do
    customer = create_customer()
    Customer.store_rpc!(customer, "air", "id")
    assert Customer.rpc_imported?(customer, "air", "id") == true
  end

  test "delete old rpcs" do
    rpc1 = insert_rpc(-:timer.minutes(1))
    rpc2 = insert_rpc(:timer.minutes(1))
    assert Customer.delete_old_rpcs() == :ok
    assert Repo.get(Schemas.AirRPC, rpc1.id) == nil
    assert Repo.get(Schemas.AirRPC, rpc2.id) != nil
  end

  defp insert_rpc(msecs_after_cleanup_boundary) do
    mtime =
      NaiveDateTime.utc_now()
      |> NaiveDateTime.add(-Application.fetch_env!(:central, :delete_air_rpcs_after), :millisecond)
      |> NaiveDateTime.add(msecs_after_cleanup_boundary * :timer.hours(24), :millisecond)

    Repo.insert!(%Schemas.AirRPC{id: Ecto.UUID.generate(), inserted_at: mtime, updated_at: mtime})
  end

  defp create_customer(name \\ "default customer") do
    assert {:ok, customer} = Repo.insert(Schemas.Customer.changeset(%Schemas.Customer{}, %{name: name}))
    customer
  end
end
