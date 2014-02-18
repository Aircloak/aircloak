require 'spec_helper'

describe PendingResult do
  before(:each) do
    PendingResult.delete_all
  end

  it "should be assigned an auth token upon creation" do
    PendingResult.create.auth_token.should_not eq nil
  end

  it "should have a unique auth token" do
    TokenGenerator.should_receive(:token_of_length).and_return("a", "a", "aa", "a", "aa", "aaa")
    PendingResult.create.auth_token.should eq "a"
    PendingResult.create.auth_token.should eq "aa"
    PendingResult.create.auth_token.should eq "aaa"
  end
end
