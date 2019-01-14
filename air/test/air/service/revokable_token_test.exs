defmodule Air.Service.RevokableToken.Test do
  use Air.SchemaCase, async: false

  alias Air.Service.RevokableToken

  describe ".sign/.verify" do
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
