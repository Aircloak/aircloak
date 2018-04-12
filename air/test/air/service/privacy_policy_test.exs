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

  describe "set" do
    test "creates when none existed" do
      assert privacy_policy_count() == 0
      PrivacyPolicy.set("content")
      assert privacy_policy_count() == 1
    end

    test "altering content results in creation of new policy" do
      create_privacy_policy!()
      count_before = privacy_policy_count()
      PrivacyPolicy.set("new content")
      assert privacy_policy_count() == count_before + 1
    end
  end

  describe "get" do
    test "returns error when no policy has been created",
      do: assert({:error, :no_privacy_policy_created} = PrivacyPolicy.get())

    test "returns the most recent policy" do
      create_privacy_policy!()
      content = "More recent privacy policy: #{:erlang.unique_integer()}"
      PrivacyPolicy.set(content)
      {:ok, privacy_policy} = PrivacyPolicy.get()
      assert privacy_policy.content == content
    end
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp privacy_policy_count(), do: Repo.aggregate(Air.Schemas.PrivacyPolicy, :count, :id)
end
