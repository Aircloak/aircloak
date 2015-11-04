require 'spec_helper'

describe UserToken do
  before(:each) do
    Analyst.destroy_all
    User.destroy_all
    UserToken.destroy_all
    RepeatedAnswer.delete_all
  end

  let(:user) {
    analyst = Analyst.create name: "test analyst"
    user = User.create login: "test", email: "test@aircloak.com", analyst: analyst, password: "1234", password_confirmation: "1234"
  }

  it "generates and retrieves a token" do
    token = UserToken.create_api_token(user)
    UserToken.api_user(token.token).should eq user
    UserToken.user(token.token, 2).should eq nil
  end
end
