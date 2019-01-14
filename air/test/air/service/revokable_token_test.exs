defmodule Air.Service.RevokableToken.Test do
  use Air.SchemaCase, async: false

  alias Air.Service.RevokableToken

  describe ".sign/.verify" do
    test "generates tokens with the given data", %{user: user} do
      token = RevokableToken.sign(%{:some => :data}, user, :session)
      assert {:ok, %{:some => :data}} = RevokableToken.verify(token, :session)
    end

    test "uses the given domain", %{user: user} do
      token = RevokableToken.sign(%{:some => :data}, user, :session)
      assert {:error, :invalid_token} = RevokableToken.verify(token, :password_reset)
    end

    @tag :pending
    test "verifies age" do
      token = RevokableToken.sign(%{:some => :data}, :session, valid_until: ~N[2019-01-01 12:00:00])
      assert {:error, :invalid_token} = RevokableToken.verify(token, :session, now: ~N[2019-01-01 12:22:33])
    end

    @tag :pending
    test "can be revoked" do
      token = RevokableToken.sign(%{:some => :data}, "some domain")
      RevokableToken.revoke(token, "some domain")

      assert {:error, :invalid_token} = RevokableToken.verify(token, "some domain")
    end

    setup do
      {:ok, user: Air.TestRepoHelper.create_user!()}
    end
  end

  describe ".revoke_all" do
    @tag :pending
    test "revokes all tokens with the given owner and type"

    @tag :pending
    test "doesn't revoke tokens with no owner"

    @tag :pending
    test "doesn't revoke tokens with another owner"

    @tag :pending
    test "doesn't revoke tokens with another type"
  end
end
