require 'spec_helper'

describe User do
  it "attempt to create a legible name from the login" do
    User.new(login: "root").attempt_to_make_a_human_name_from_login.should eq "Root"
    User.new(login: "root.me").attempt_to_make_a_human_name_from_login.should eq "Root Me"
    User.new(login: "f.broth").attempt_to_make_a_human_name_from_login.should eq "F Broth"
  end
end
