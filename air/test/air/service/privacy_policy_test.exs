defmodule Air.Service.PrivacyPolicyTest do
  use Air.SchemaCase, async: true

  import Air.TestRepoHelper
  alias Air.{Repo, Service.PrivacyPolicy}

  describe "exists?" do
    test "no privacy policy", do: refute(PrivacyPolicy.exists?())

    test "privacy policy created" do
      create_privacy_policy!()
      assert PrivacyPolicy.exists?()
    end
  end
end
