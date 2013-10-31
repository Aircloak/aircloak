require 'spec_helper'

describe DeployableEntityVersion do
  before(:each) do
    DeployableEntityVersion.destroy_all
    DeployableEntity.destroy_all
    Build.destroy_all
    @d = nil
    VCR.use_cassette('entity-create-erlattest', allow_playback_repeats: true) do
      @d = DeployableEntity.create(
        repo: "erlattest",
        tpm_env: "tpm",
        no_tpm_env: "no-tpm"
      )
    end
  end

  describe "associations" do
    it "should not save without a deployable_entity" do
      d = DeployableEntityVersion.new
      d.save.should eq(false)
      d.errors.messages[:deployable_entity_id].should_not eq(nil)
    end
  end

  describe "validations" do
    it "should have a unique commit id" do
      Gh.should_receive(:add_message_and_author)
      commit = "commit_id"
      d = DeployableEntityVersion.new commit_id: commit, deployable_entity_id: @d.id
      VCR.use_cassette('create-deployable-entity-version') do
        d.save.should eq(true)
      end
      d = DeployableEntityVersion.new commit_id: commit, deployable_entity_id: @d.id
      d.save.should eq(false)
    end
  end

  it "should set the commit message" do
    commit = "0253132d55e40e3f8757b4e3dda6d6967fc90726"
    VCR.use_cassette('erlattest-commit-message') do
      @d.add_commit(commit)
    end

    msg = "bug fix.\n\nLooking for aikblob in the wrong place " +
        "(holdover from where\nprivacyca function used to put it)"
    DeployableEntityVersion.find_by_commit_id(commit).message.should eq msg
  end

  it "should know if it is part of a build" do
    deployable_entity_version = PreRecorded.setup_deployable_entity_version @d
    deployable_entity_version.part_of_build?.should eq false
    b = Build.create name: "test"
    b.deployable_entity_versions << deployable_entity_version
    deployable_entity_version.part_of_build?.should eq true
  end

  it "should be able to tell a short version of a commit" do
    d = DeployableEntityVersion.new
    d.commit_id = "1234567890abcdefghijklmnopqr"
    d.short_commit_id.should eq "1234567890"
  end
end
