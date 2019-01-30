defmodule Air.Service.RevokableToken.Test do
  use Air.SchemaCase, async: false

  alias Air.Service.RevokableToken

  setup do
    {:ok, user: Air.TestRepoHelper.create_user!()}
  end

  describe ".sign/.verify/.revoke" do
    test "generates tokens with the given data", %{user: user} do
      token = RevokableToken.sign(%{:some => :data}, user, :session, :infinity)
      assert {:ok, %{:some => :data}} = RevokableToken.verify(token, :session)
    end

    test "uses the given domain", %{user: user} do
      token = RevokableToken.sign(%{:some => :data}, user, :password_reset, :infinity)
      assert {:error, :invalid_token} = RevokableToken.verify(token, :session)
    end

    test "rejects old tokens", %{user: user} do
      token = RevokableToken.sign(%{:some => :data}, user, :session, div(:timer.hours(1), :timer.seconds(1)))

      assert {:error, :invalid_token} =
               RevokableToken.verify(token, :session,
                 now: NaiveDateTime.utc_now() |> NaiveDateTime.add(:timer.hours(2), :millisecond)
               )
    end

    test "rejects nil tokens" do
      assert {:error, :invalid_token} = RevokableToken.verify(nil, :session)
    end

    test "allows tokens within their validity period", %{user: user} do
      token = RevokableToken.sign(%{:some => :data}, user, :session, div(:timer.hours(1), :timer.seconds(1)))

      assert {:ok, %{:some => :data}} =
               RevokableToken.verify(token, :session,
                 now: NaiveDateTime.utc_now() |> NaiveDateTime.add(:timer.minutes(30), :millisecond)
               )
    end

    test "can be revoked", %{user: user} do
      token = RevokableToken.sign(%{:some => :data}, user, :session, :infinity)
      RevokableToken.revoke(token, :session)

      assert {:error, :invalid_token} = RevokableToken.verify(token, :session)
    end
  end

  describe ".verify_and_revoke" do
    test "does a verify/revoke combo", %{user: user} do
      token = RevokableToken.sign(%{some: :data}, user, :session, :infinity)

      assert {:ok, %{some: :data}} == RevokableToken.verify_and_revoke(token, :session)
      assert {:error, :invalid_token} = RevokableToken.verify(token, :session)
    end
  end

  describe ".revoke_all" do
    test "revokes all tokens with the given owner and type", %{user: user} do
      token = RevokableToken.sign(%{:some => :data}, user, :session, :infinity)
      RevokableToken.revoke_all(user, :session)
      assert {:error, :invalid_token} = RevokableToken.verify(token, :session)
    end

    test "doesn't revoke tokens with another owner", %{user: user} do
      token = RevokableToken.sign(%{:some => :data}, Air.TestRepoHelper.create_user!(), :session, :infinity)
      RevokableToken.revoke_all(user, :session)
      assert {:ok, _} = RevokableToken.verify(token, :session)
    end

    test "doesn't revoke tokens with another type", %{user: user} do
      token = RevokableToken.sign(%{:some => :data}, user, :password_reset, :infinity)
      RevokableToken.revoke_all(user, :session)
      assert {:ok, _} = RevokableToken.verify(token, :password_reset)
    end
  end

  describe ".count" do
    test "counts the users tokens", %{user: user} do
      RevokableToken.sign(:data, user, :session, :infinity)
      RevokableToken.sign(:data, user, :session, :infinity)

      assert RevokableToken.count(user, :session) == 2
    end

    test "does not count other types", %{user: user} do
      RevokableToken.sign(:data, user, :password_reset, :infinity)
      assert RevokableToken.count(user, :session) == 0
    end

    test "does not count other user's tokens", %{user: user} do
      RevokableToken.sign(:data, Air.TestRepoHelper.create_user!(), :session, :infinity)
      assert RevokableToken.count(user, :session) == 0
    end
  end

  describe ".cleanup" do
    test "removes tokens with validity in the past", %{user: user} do
      token = RevokableToken.sign(:data, user, :session, 10)
      RevokableToken.cleanup(NaiveDateTime.utc_now() |> NaiveDateTime.add(20))
      assert {:error, :invalid_token} = RevokableToken.verify(token, :session)
    end

    test "does not remove still valid tokens", %{user: user} do
      token = RevokableToken.sign(:data, user, :session, 10)
      RevokableToken.cleanup()
      assert {:ok, :data} = RevokableToken.verify(token, :session)
    end
  end
end
