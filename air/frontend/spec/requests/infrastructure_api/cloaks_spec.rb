require 'spec_helper'
require 'json'

describe InfrastructureApi::CloaksController do
  before(:each) do
    Cloak.delete_all
  end

  describe "GET /infrastructure-api/cloaks" do
    it "should return a list cloaks as a CSV" do
      Cloak.create(
        name: "test1",
        ip: "1.2.3.4",
        tpm: true,
        good: true
      )
      Cloak.create(
        name: "test2",
        ip: "2.3.4.5",
        tpm: false,
        good: false
      )

      get "/infrastructure-api/cloaks"

      response.status.should be(200)

      expected_result = "test1;1.2.3.4;true;true\ntest2;2.3.4.5;false;false"
      response.body.should eq expected_result
    end
  end
end
