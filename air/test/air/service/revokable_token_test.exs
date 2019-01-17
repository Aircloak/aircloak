defmodule Air.Service.RevokableToken.Test do
  use Air.SchemaCase, async: false

  alias Air.Service.RevokableToken

  setup do
    {:ok, user: Air.TestRepoHelper.create_user!()}
  end

  describe ".sign/.verify/.revoke" do
    test "generates tokens with the given data", %{user: user} do
      token = RevokableToken.sign(%{:some => :data}, user, :session)
      assert {:ok, %{:some => :data}} = RevokableToken.verify(token, :session, max_age: :infinity)
    end

    test "uses the given domain", %{user: user} do
      token = RevokableToken.sign(%{:some => :data}, user, :password_reset)
      assert {:error, :invalid_token} = RevokableToken.verify(token, :session, max_age: :infinity)
    end

    test "rejects old tokens", %{user: user} do
      token = RevokableToken.sign(%{:some => :data}, user, :session)

      assert {:error, :invalid_token} =
               RevokableToken.verify(token, :session,
                 max_age: :timer.hours(1) / :timer.seconds(1),
                 now: NaiveDateTime.utc_now() |> NaiveDateTime.add(:timer.hours(2), :millisecond)
               )
    end

    test "allows tokens within max_age", %{user: user} do
      token = RevokableToken.sign(%{:some => :data}, user, :session)

      assert {:ok, %{:some => :data}} =
               RevokableToken.verify(token, :session,
                 max_age: :timer.hours(1) / :timer.seconds(1),
                 now: NaiveDateTime.utc_now() |> NaiveDateTime.add(:timer.minutes(30), :millisecond)
               )
    end

    test "can be revoked", %{user: user} do
      token = RevokableToken.sign(%{:some => :data}, user, :session)
      RevokableToken.revoke(token, :session)

      assert {:error, :invalid_token} = RevokableToken.verify(token, :session, max_age: :infinity)
    end
  end

  describe ".verify_and_revoke" do
    test "does a verify/revoke combo", %{user: user} do
      token = RevokableToken.sign(%{some: :data}, user, :session)

      assert {:ok, %{some: :data}} == RevokableToken.verify_and_revoke(token, :session, max_age: :infinity)
      assert {:error, :invalid_token} = RevokableToken.verify(token, :session, max_age: :infinity)
    end
  end

  describe ".revoke_all" do
    test "revokes all tokens with the given owner and type", %{user: user} do
      token = RevokableToken.sign(%{:some => :data}, user, :session)
      RevokableToken.revoke_all(user, :session)
      assert {:error, :invalid_token} = RevokableToken.verify(token, :session, max_age: :infinity)
    end

    test "doesn't revoke tokens with another owner", %{user: user} do
      token = RevokableToken.sign(%{:some => :data}, Air.TestRepoHelper.create_user!(), :session)
      RevokableToken.revoke_all(user, :session)
      assert {:ok, _} = RevokableToken.verify(token, :session, max_age: :infinity)
    end

    test "doesn't revoke tokens with another type", %{user: user} do
      token = RevokableToken.sign(%{:some => :data}, user, :password_reset)
      RevokableToken.revoke_all(user, :session)
      assert {:ok, _} = RevokableToken.verify(token, :password_reset, max_age: :infinity)
    end
  end
end
