require 'spec_helper'

describe AnalystToken do
  before(:each) do
    Analyst.destroy_all
    User.destroy_all
    AnalystToken.destroy_all
    RepeatedAnswer.delete_all
  end

  let(:user) {
    analyst = Analyst.create name: "test analyst"
    user = User.create login: "test", email: "test@aircloak.com", analyst: analyst, password: "1234", password_confirmation: "1234"
  }

  it "generates and retrieves a token" do
    token = AnalystToken.create_api_token(user)
    AnalystToken.api_user(token.token).should eq user
    AnalystToken.user(token.token, 2).should eq nil
  end
end
