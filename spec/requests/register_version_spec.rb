require 'spec_helper'

describe RegisterVersionController do
  before(:each) do
  end

  describe "POST /register_version" do
    it "should ignore build notifications from travis-ci" do
      count_before = DeployableEntityVersion.count
      post '/register_version', travis_payload, {'Authorization' => "2c2ff48b0e0349e91abe1dcb825fc81981448ec9ca7b954c5641353ef15e1fc4"}
      DeployableEntityVersion.count.should eq count_before
      response.status.should be(200)
    end
  end
end

def travis_payload
  open("spec/test_data/travis-ci-payload").read
end
