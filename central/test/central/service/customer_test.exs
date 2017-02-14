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

  test "storing air status" do
    customer = create_customer()
    assert :ok == Customer.mark_air_online(customer, "air1", air_info([]))
    assert :online == air_data(customer, "air1").status
    assert :online == air_data(customer, "air1").status
  end

  test "marking an air as online also sets the air version" do
    customer = create_customer()
    assert :ok == Customer.mark_air_online(customer, "air1", air_info([]))
    assert @air_version == air_data(customer, "air1").version
  end

  test "storing air status with online cloaks" do
    customer = create_customer()
    assert :ok == Customer.mark_air_online(customer, "air1", air_info())
    assert :online == air_data(customer, "air1").status
    assert :online == cloak_data(customer, "air1", "cloak1").status
    assert :online == cloak_data(customer, "air1", "cloak2").status
  end

  test "storing air status with online cloaks updates cloak version" do
    customer = create_customer()
    assert :ok == Customer.mark_air_online(customer, "air1", air_info())
    assert @cloak1_version == cloak_data(customer, "air1", "cloak1").version
    assert @cloak2_version == cloak_data(customer, "air1", "cloak2").version
  end

  test "updating air status" do
    customer = create_customer()
    Customer.mark_air_online(customer, "air1", air_info([]))
    Customer.mark_air_offline(customer, "air1")
    assert :offline == air_data(customer, "air1").status
  end

  test "updating air status to offline will set cloaks to offline as well" do
    customer = create_customer()
    Customer.mark_air_online(customer, "air1", air_info())
    Customer.mark_air_offline(customer, "air1")
    assert :offline == cloak_data(customer, "air1", "cloak1").status
    assert :offline == cloak_data(customer, "air1", "cloak2").status
  end

  test "resetting air statuses" do
    customer = create_customer()
    Customer.mark_air_online(customer, "air1", air_info())
    Customer.mark_air_online(customer, "air2", air_info([]))
    Customer.reset_air_statuses()
    assert :offline == air_data(customer, "air1").status
    assert :offline == air_data(customer, "air2").status
    assert :offline == cloak_data(customer, "air1", "cloak1").status
    assert :offline == cloak_data(customer, "air1", "cloak2").status
  end

  test "storing cloak status" do
    customer = create_customer()
    assert :ok == Customer.mark_air_online(customer, "air1", air_info([]))
    assert :ok == Customer.update_cloak(customer, "air1", "cloak1", status: :online)
    assert :online == cloak_data(customer, "air1", "cloak1").status
  end

  test "updating cloak status" do
    customer = create_customer()
    assert :ok == Customer.mark_air_online(customer, "air1", air_info())
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


  defp air_info(online_cloaks \\ standard_online_cloaks()), do:
    %{
      air_version: @air_version,
      online_cloaks: online_cloaks,
    }

  defp standard_online_cloaks(), do:
    [
      %{
        name: "cloak1",
        version: @cloak1_version,
        data_source_names: ["ds1"],
      }, %{
        name: "cloak2",
        version: @cloak2_version,
        data_source_names: ["ds1", "ds2"],
      }
    ]
end
