require 'spec_helper'
require 'json'

describe InfrastructureApi::AuthenticatedController do
  setup :activate_authlogic

  before(:each) do
    Analyst.delete_all
  end

  after(:all) do
    Analyst.delete_all
  end

  it "should know if user not logged in" do
    get "/infrastructure-api/authenticated"
    response.status.should eq 200
    json = JSON.parse(response.body)
    json["authenticated"].should eq false
  end

  it "should return user id and analyst id if user logged in" do
    analyst = Analyst.create(name: "test-analyst")
    user = create_user admin: false, email: "user@example.com", analyst: analyst
    log_in(user)
    get "/infrastructure-api/authenticated"
    response.status.should eq 200
    json = JSON.parse(response.body)
    json["authenticated"].should eq true
    json["user_id"].should eq user.id
    json["analyst_id"].should eq analyst.id
  end
end
