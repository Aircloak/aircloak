require 'spec_helper'

describe RegisterVersionController do
  before(:each) do
    DeployableEntity.destroy_all
    DeployableEntityVersion.destroy_all
    VersionTest.stub(:new_from_deployable_entity_version)
  end

  describe "POST /register_version" do
    it "should register builds from travis-ci" do
      Gh.should_receive(:add_message_and_author)

      # For this we need the erlattest repo
      VCR.use_cassette('entity-save-erlattest', allow_playback_repeats: true) do
        DeployableEntity.create(
          repo: "erlattest"
        )
      end

      count_before = DeployableEntityVersion.count
      post '/register_version', travis_payload, {'Authorization' => "2c2ff48b0e0349e91abe1dcb825fc81981448ec9ca7b954c5641353ef15e1fc4"}
      DeployableEntityVersion.count.should eq(count_before + 1)
      response.status.should be(200)
    end
  end

  describe "unauthorized POST /register_version" do
    it "should register builds from travis-ci" do
      count_before = DeployableEntityVersion.count
      post '/register_version', travis_payload, {'Authorization' => "invalid_hash"}
      DeployableEntityVersion.count.should eq count_before
      response.status.should be(401)
    end
  end
end

def travis_payload
  open("spec/test_data/travis-ci-payload").read
end
