defmodule Air.Service.License.FSM.Test do
  use ExUnit.Case, async: true

  alias Air.Service.License.FSM

  test "initial state" do
    state = FSM.initial()

    refute FSM.present?(state)
    refute FSM.valid?(state)
    assert Timex.diff(FSM.expiry(state), Timex.now()) < 0
    assert "" = FSM.text(state)
  end

  describe "load" do
    setup [:load_public_key, :load_valid_license, :load_expired_license]

    test "invalid license", %{public_key: public_key}, do:
      assert FSM.initial() |> FSM.load(public_key, invalid_license()) == {:error, FSM.initial()}

    test "valid license", %{public_key: public_key, valid_license: valid_license} do
      {:ok, state} = FSM.initial() |> FSM.load(public_key, valid_license)

      assert FSM.present?(state)
      assert FSM.valid?(state)
      assert Timex.diff(FSM.expiry(state), Timex.now()) > 0
      assert FSM.text(state) == valid_license
    end

    test "text contains multiple licenses, one of which is valid (used for rotating keys)",
      %{public_key: public_key, valid_license: valid_license}
    do
      license_text = invalid_license() <> "\n" <> valid_license
      {:ok, state} = FSM.initial() |> FSM.load(public_key, license_text)

      assert FSM.present?(state)
      assert FSM.text(state) == license_text
    end

    test "expired license", %{public_key: public_key, expired_license: expired_license} do
      {:ok, state} = FSM.initial() |> FSM.load(public_key, expired_license)

      assert FSM.present?(state)
      refute FSM.valid?(state)
      assert Timex.diff(FSM.expiry(state), Timex.now()) < 0
      assert FSM.text(state) == expired_license
    end

    test "loading another license overwrites the previous one",
      %{public_key: public_key, valid_license: valid_license, expired_license: expired_license}
    do
      {:ok, state1} = FSM.initial() |> FSM.load(public_key, expired_license)
      {:ok, state2} = FSM.load(state1, public_key, valid_license)

      assert FSM.text(state1) != FSM.text(state2)
    end
  end

  defp invalid_license(), do:
    """
    Some random text
    More random text
    """

  defp load_valid_license(_context), do: {:ok, valid_license: File.read!("priv/dev_license.lic")}

  defp load_expired_license(_context), do: {:ok, expired_license: File.read!("priv/expired_dev_license.lic")}

  defp load_public_key(_context) do
    {:ok, key} =Application.get_env(:air, :license) |> Keyword.fetch!(:public_key) |> ExPublicKey.load()
    {:ok, public_key: key}
  end
end
