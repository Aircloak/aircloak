require 'spec_helper'

describe RegisterVersionController do
  before(:each) do
    DeployableEntity.destroy_all
    DeployableEntityVersion.destroy_all
    VersionTest.stub(:new_from_deployable_entity_version)
  end

  describe "POST /register_versin" do
    it "should register builds from travis-ci" do
      Gh.should_receive(:add_message_and_author)

      # For this we need the erlattest repo
      VCR.use_cassette('entity-save-erlattest', allow_playback_repeats: true) do
        DeployableEntity.create(
          repo: "erlattest",
          tpm_env: "tpm",
          no_tpm_env: "no-tpm"
        )
      end

      count_before = DeployableEntityVersion.count
      post '/register_version', travis_payload
      DeployableEntityVersion.count.should eq(count_before + 1)
      response.status.should be(200)
    end
  end
end

def travis_payload
  open("spec/test_data/travis-ci-payload").read
end
