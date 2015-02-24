require 'spec_helper'

describe User do
  before :each do
    Permission.delete_all
    Permission.create name: :admin
    User.delete_all
  end

  it "attempt to create a legible name from the login" do
    User.new(login: "root").attempt_to_make_a_human_name_from_login.should eq "Root"
    User.new(login: "root.me").attempt_to_make_a_human_name_from_login.should eq "Root Me"
    User.new(login: "f.broth").attempt_to_make_a_human_name_from_login.should eq "F Broth"
  end

  it "should prevent non-aircloakers from being admin" do
    u = User.new(login: "test", email: "hello@example.com", password: "1234",
        password_confirmation: "1234")
    u.permission_ids << Permission.find_by_name("admin").id
    u.save.should eq false
    u.errors[:base].count.should_not eq 0
  end

  it "should allow aircloakers to be admins" do
    u = User.new(login: "test", email: "sebastian@aircloak.com", password: "1234",
        password_confirmation: "1234")
    u.permission_ids << Permission.find_by_name("admin").id
    u.save.should eq true
  end
end
