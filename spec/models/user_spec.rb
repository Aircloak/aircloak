require 'spec_helper'

describe User do
  before :each do
    Analyst.delete_all
    Permission.delete_all
    Permission.create name: :admin
    Analyst.create name: "test_analyst"
    User.delete_all
  end

  def user params = {}
    # We are NOT using the create_user helper method in order to
    # be able to validate the behaviour that only aircloakers can
    # be administrators, which the spec helper attempts to hide
    # from other tests.
    u = User.new(
      login: (params.delete(:login) || "test"),
      email: (params.delete(:email) || "example@example.com"),
      password: "1234",
      password_confirmation: "1234",
      analyst: Analyst.first
    )
  end

  it "attempt to create a legible name from the login" do
    User.new(login: "root").attempt_to_make_a_human_name_from_login.should eq "Root"
    User.new(login: "root.me").attempt_to_make_a_human_name_from_login.should eq "Root Me"
    User.new(login: "f.broth").attempt_to_make_a_human_name_from_login.should eq "F Broth"
  end

  it "should prevent non-aircloakers from being admin" do
    u = user email: "example@example.com"
    u.permission_ids = [Permission.find_by_name("admin").id]
    u.save.should eq false
    u.errors[:base].count.should_not eq 0

    u = user email: "example@aircloak.com.foobar.com"
    u.permission_ids = [Permission.find_by_name("admin").id]
    u.save.should eq false
    u.errors[:base].count.should_not eq 0
  end

  it "should allow aircloakers to be admins" do
    u = user email: "user@aircloak.com"
    u.permission_ids = [Permission.find_by_name("admin").id]
    u.save.should eq true
  end

  let(:admin_params) do
    p = Permission.find_by_name :admin
    {
      "login"=>"username",
      "email"=>"username@aircloak.com",
      "password"=>"12345",
      "password_confirmation"=>"12345",
      "analyst_id"=>"none",
      "permission_ids"=>["#{p.id}", p.id]
    }
  end

  it "should not be possible to assign admin rights from a non-admin account" do
    u = user email: "non-admin@example.com"
    new_user = u.new_user_from_params admin_params, "none"
    p = Permission.find_by_name :admin
    new_user.permission_ids.include?(p.id).should eq false
  end

  it "should be possible for an admin user to assign admin rights" do
    u = user email: "user@aircloak.com"
    p = Permission.find_by_name :admin
    u.permissions << p
    new_user = u.new_user_from_params admin_params, "none"
    new_user.permission_ids.include?(p.id).should eq true
  end

  describe "scoping" do
    it "should not list impersonating users under managed users" do
      impersonator = user login: "admin-user", email: "user@aircloak.com"
      p = Permission.create name: "admin"
      impersonator.permissions << p
      impersonator.save.should eq true

      non_admin = user
      non_admin.save.should eq true

      impersonator.analyst.should eq non_admin.analyst

      impersonator.managed_users.should include non_admin
      non_admin.managed_users.should_not include impersonator
    end

    it "should not include impersonating users when doing a scoped find for non-admin users" do
      impersonator = user login: "admin-user", email: "user@aircloak.com"
      p = Permission.create name: "admin"
      impersonator.permissions << p
      impersonator.save.should eq true

      non_admin = user
      non_admin.save.should eq true

      impersonator.analyst.should eq non_admin.analyst

      expect {non_admin.scoped_find impersonator.id}.to raise_exception ActiveRecord::RecordNotFound
    end

    it "should not include users from other analysts in a scoped find for non-admin users" do
      user1 = user login: "user1", email: "user1@example.com"

      a = Analyst.create name: "test_analyst2"
      user2 = user login: "user2", email: "user2@example.com"
      user2.analyst = a

      user1.save.should eq true
      user2.save.should eq true
      user1.analyst.should_not eq user2.analyst

      expect {user1.scoped_find user2.id}.to raise_exception ActiveRecord::RecordNotFound
    end

  end
end
