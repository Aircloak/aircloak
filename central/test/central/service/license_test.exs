defmodule Central.Service.License.Test do
  use Central.ModelCase, async: true

  alias Central.Schemas
  alias Central.Service.License

  setup_all [:read_public_key]
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

  test "creating an invalid license", %{customer: customer}, do:
    assert {:error, _} = License.create(customer, %{name: ""})

  test "exporting a non-existent license", %{customer: customer}, do:
    assert :not_found = License.export(customer, 123)

  test "exported license includes customer id", %{customer: customer, public_key: public_key} do
    {:ok, license} = License.create(customer, %{name: "some license", length_in_days: 10, auto_renew: true})

    {:ok, text} = License.export(customer, license.id)

    customer_id = customer.id
    assert {:ok, %{"customer_id" => ^customer_id}} = decode(text, public_key)
  end

  test "exporting an auto-renew license", %{customer: customer, public_key: public_key} do
    {:ok, license} = License.create(customer, %{name: "some license", length_in_days: 10, auto_renew: true})

    {:ok, text} = License.export(customer, license.id)

    assert {:ok, %{"expires_at" => expires_at}} = decode(text, public_key)
    assert {:ok, expires_at} = Timex.parse(expires_at, "{ISO:Basic}")
    assert Timex.diff(expires_at, Timex.now(), :days) >= 9
    assert Timex.diff(expires_at, Timex.now(), :days) <= 11
  end

  test "exporting a non-auto-renew license", %{customer: customer, public_key: public_key} do
    {:ok, license} = License.create(customer, %{name: "some license", length_in_days: 10, auto_renew: false})
    creation = Timex.now() |> Timex.shift(days: -100)
    Ecto.Adapters.SQL.query!(Repo, "UPDATE licenses SET inserted_at = $1 WHERE id = $2", [creation, license.id])

    {:ok, text} = License.export(customer, license.id)

    assert {:ok, %{"expires_at" => expires_at}} = decode(text, public_key)
    assert {:ok, expires_at} = Timex.parse(expires_at, "{ISO:Basic}")
    assert Timex.diff(expires_at, creation, :days) >= 9
    assert Timex.diff(expires_at, creation, :days) <= 11
  end

  test "exporting a specific license", %{customer: customer, public_key: public_key} do
    {:ok, _} = License.create(customer, %{name: "some license", length_in_days: 10, auto_renew: true})
    {:ok, license} = License.create(customer, %{name: "some license", length_in_days: 10, auto_renew: true})

    {:ok, text} = License.export(customer, license.id)

    assert {:ok, %{"id" => license_id}} = decode(text, public_key)
    assert license_id == license.id
  end

  defp setup_customer(_) do
    {:ok, %{customer: create_customer()}}
  end

  defp create_customer(name \\ "Some name") do
    {:ok, customer} = Repo.insert(Schemas.Customer.changeset(%Schemas.Customer{}, %{name: name}))
    customer
  end

  defp read_public_key(_) do
    root_path = Application.app_dir(:central)
    file_name = Application.get_env(:central, :license) |> Keyword.fetch!(:public_key)
    {:ok, key} = ExPublicKey.load(Path.join([root_path, file_name]))
    {:ok, public_key: key}
  end

  defp decode(cipher_text, public_key) do
    with {:ok, plain} <- ExPublicKey.decrypt_public(cipher_text, public_key) do
      Poison.decode(plain)
    end
  end
end
