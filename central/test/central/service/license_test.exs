defmodule Central.Service.License.Test do
  use Central.ModelCase, async: true

  alias Central.Schemas
  alias Central.Service.License

  setup_all [:read_public_key]
  setup [:setup_customer]

  describe "creation and listing" do
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
  end

  describe "export" do
    test "exported license includes customer and license id", %{customer: customer, public_key: public_key} do
      {:ok, license} = License.create(customer, %{name: "some license", length_in_days: 10, auto_renew: true})

      text = License.export(license)

      customer_id = customer.id
      license_id = license.id

      assert {:ok, %{"customer_id" => ^customer_id, "id" => ^license_id}} = decode(text, public_key)
    end

    test "exporting an auto-renew license", %{customer: customer, public_key: public_key} do
      {:ok, license} = License.create(customer, %{name: "some license", length_in_days: 10, auto_renew: true})

      text = License.export(license)

      assert {:ok, %{"expires_at" => expires_at, "auto_renew" => true}} = decode(text, public_key)
      assert {:ok, expires_at} = Timex.parse(expires_at, "{ISO:Basic}")
      assert Timex.diff(expires_at, Timex.now(), :days) >= 9
      assert Timex.diff(expires_at, Timex.now(), :days) <= 11
    end

    test "exporting a non-auto-renew license", %{customer: customer, public_key: public_key} do
      {:ok, license} = License.create(customer, %{name: "some license", length_in_days: 10, auto_renew: false})

      creation = Timex.now() |> Timex.shift(days: -100)

      text = License.export(%{license | inserted_at: creation})

      assert {:ok, %{"expires_at" => expires_at, "auto_renew" => false}} = decode(text, public_key)
      assert {:ok, expires_at} = Timex.parse(expires_at, "{ISO:Basic}")
      assert Timex.diff(expires_at, creation, :days) >= 9
      assert Timex.diff(expires_at, creation, :days) <= 11
    end

    test "exporting a license with features", %{customer: customer, public_key: public_key} do
      {:ok, license} =
        License.create(customer, %{name: "some license", length_in_days: 10, auto_renew: true, features: ["ldap"]})

      assert {:ok, %{"features" => ["ldap"]}} = license |> License.export() |> decode(public_key)
    end
  end

  describe "revocation" do
    test "revoked auto-renew licenses are treated as non-auto-renew", %{customer: customer, public_key: public_key} do
      {:ok, license} = License.create(customer, %{name: "some license", length_in_days: 10, auto_renew: true})

      creation = Timex.now() |> Timex.shift(days: -100)

      {:ok, license} = License.revoke(license)
      text = License.export(%{license | inserted_at: creation})

      assert {:ok, %{"expires_at" => expires_at, "auto_renew" => false}} = decode(text, public_key)
      assert {:ok, expires_at} = Timex.parse(expires_at, "{ISO:Basic}")
      assert Timex.diff(expires_at, creation, :days) >= 9
      assert Timex.diff(expires_at, creation, :days) <= 11
    end

    test "revoking a license", %{customer: customer} do
      {:ok, license} = License.create(customer, %{name: "some license", length_in_days: 1, auto_renew: true})

      {:ok, _} = License.revoke(license)

      assert %{revoked: true} = Repo.get(Schemas.License, license.id)
    end

    test "restoring a license", %{customer: customer} do
      {:ok, license} = License.create(customer, %{name: "some license", length_in_days: 1, auto_renew: true})

      {:ok, license} = License.revoke(license)
      {:ok, _} = License.restore(license)

      assert %{revoked: false} = Repo.get(Schemas.License, license.id)
    end
  end

  describe "renewal" do
    test "invalid license", do: assert({:error, :invalid_license} = License.renew("invalid"))

    test "valid license", %{customer: customer, public_key: public_key} do
      {:ok, license} = License.create(customer, %{name: "some license", length_in_days: 1, auto_renew: true})

      old_text = License.export(license)
      {:ok, new_text} = License.renew(old_text)

      {:ok, %{"expires_at" => old_expires_at}} = decode(old_text, public_key)
      {:ok, %{"expires_at" => new_expires_at}} = decode(new_text, public_key)
      {:ok, old_expires_at} = Timex.parse(old_expires_at, "{ISO:Basic}")
      {:ok, new_expires_at} = Timex.parse(new_expires_at, "{ISO:Basic}")

      assert Timex.diff(new_expires_at, old_expires_at) > 0
    end
  end

  defp setup_customer(_) do
    {:ok, %{customer: create_customer()}}
  end

  defp create_customer(name \\ "Some name"),
    do: Repo.insert!(Schemas.Customer.changeset(%Schemas.Customer{}, %{name: name}))

  defp read_public_key(_) do
    root_path = Application.app_dir(:central)
    file_name = Application.get_env(:central, :license) |> Keyword.fetch!(:private_key)
    {:ok, key} = ExPublicKey.load(Path.join([root_path, file_name]))
    {:ok, key} = ExPublicKey.public_key_from_private_key(key)

    {:ok, public_key: key}
  end

  defp decode(cipher_text, public_key) do
    with {:ok, plain} <- ExPublicKey.decrypt_public(cipher_text, public_key) do
      Poison.decode(plain)
    end
  end
end
