defmodule Air.Service.License.Test do
  # because of shared mode
  use Air.SchemaCase, async: false

  alias Air.Service.License

  @missing_license "priv/missing_license.lic"
  @valid_license1 "priv/dev_license.lic"
  @valid_license2 "priv/license_without_auto_renew.lic"
  @broken_license "priv/broken_license.lic"
  @priv_from_test "../../"

  describe "load_from_file" do
    test "missing license" do
      assert {:error, :enoent} == License.load_from_file(@missing_license)
    end

    test "valid license" do
      dev_license = File.read!(@valid_license1)
      other_license = File.read!(@valid_license2)
      refute dev_license == other_license

      assert :ok == License.load_from_file(@priv_from_test <> @valid_license1)
      assert dev_license == License.text()
      assert License.present?()

      assert :ok == License.load_from_file(@priv_from_test <> @valid_license2)
      assert other_license == License.text()
      assert License.present?()
    end

    test "broken license" do
      broken_license = File.read!(@broken_license)
      assert {:error, reason} = License.load_from_file(@priv_from_test <> @broken_license)
      assert reason =~ ~r/Could not verify license file.*corrupted/
      refute broken_license == License.text()
    end
  end
end
