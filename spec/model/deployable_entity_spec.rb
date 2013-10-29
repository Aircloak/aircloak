require 'spec_helper'

describe DeployableEntity do
  def valid_params
    params_for "erlattest"
  end

  def params_for repo
    { 
      repo: repo,
      tpm_env: "tpm",
      no_tpm_env: "no_tpm"
    }
  end

  before do
    DeployableEntity.destroy_all
  end

  describe "Validations" do
    it "should require that the entities are unique" do
      VCR.use_cassette('entity-save-erlattest', allow_playback_repeats: true) do
        d1 = DeployableEntity.new valid_params
        d1.save.should eq(true)
        d2 = DeployableEntity.new valid_params
        d2.save.should eq(false)
      end
    end

    it "should only validate if the entity exists on github" do
      d1 = DeployableEntity.new params_for "foobar"
      VCR.use_cassette('entity-save-foobar', allow_playback_repeats: true) do
        d1.save.should eq(false)
      end
      d1.errors.messages[:repo].should_not eq(nil)
    end

    it "should require tpm_env" do
      VCR.use_cassette('entity-save-erlattest', allow_playback_repeats: true) do
        d = DeployableEntity.new repo: "erlattest"
        d.save.should eq(false)
        d.errors.messages[:tpm_env].should_not eq(nil)
        d.errors.messages[:no_tpm_env].should_not eq(nil)
      end
    end
  end

  describe "Maintenance" do
    it "should set the description on save" do
      VCR.use_cassette('entity-save-erlattest', allow_playback_repeats: true) do
        d1 = DeployableEntity.new valid_params
        d1.save.should eq(true)
        d1.description.should eq("Simple erlang attestation batching server.")
      end
    end

    it "should not alter the description if the repo doesn't exist" do
      VCR.use_cassette('entity-save-foobar', allow_playback_repeats: true) do
        d1 = DeployableEntity.new params_for "foobar"
        d1.description = "foo"
        d1.save.should eq(false)
        d1.description.should eq("foo")
      end
    end
  end
end
