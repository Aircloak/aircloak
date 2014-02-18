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

  before(:each) do
    begin
      BuildManager
    rescue
      class BuildManager; end
    end

    VersionTest.stub(:new_from_deployable_entity_version)
    
    Cluster.delete_all
    BuildManager.stub(:send_build_request).and_return(true)
    Build.delete_all

    DeployableEntityVersion.delete_all
    DeployableEntity.delete_all
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

    it "should allow to delete an entity without any versions" do
      d = nil
      VCR.use_cassette('entity-save-erlattest', allow_playback_repeats: true) do
        d = DeployableEntity.create params_for "erlattest"
      end
      d.can_destroy?.should eq true
      d.destroy.destroyed?.should eq true
    end

    it "should allow to delete an entity with versions if they are not used in builds" do
      d = nil
      VCR.use_cassette('entity-save-erlattest', allow_playback_repeats: true) do
        d = DeployableEntity.create params_for "erlattest"
      end
      dev = PreRecorded.setup_deployable_entity_version d
      d.deployable_entity_versions << dev
      d.can_destroy?.should eq true
      d.destroy.destroyed?.should eq true
    end

    it "should not allow to delete an entity with a version in use in a build" do
      d = nil
      VCR.use_cassette('entity-save-erlattest', allow_playback_repeats: true) do
        d = DeployableEntity.create params_for "erlattest"
      end
      dev = PreRecorded.setup_deployable_entity_version d
      dev.builds << Build.new(name: "super build")
      d.deployable_entity_versions << dev

      d.can_destroy?.should eq false
      d.destroy.should eq false
      d.destroyed?.should eq false
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

  describe "Version" do
    it "should be possible to add versions to an entity" do
      Gh.should_receive(:add_message_and_author)

      VCR.use_cassette('entity-save-erlattest', allow_playback_repeats: true) do
        d = DeployableEntity.new valid_params
        d.save.should eq(true)

        # Fine, now we have a valid entity
        commit = "commit_id"
        d.add_commit commit
        # Unique on commit_id, so no risk of finding multiple
        c = DeployableEntityVersion.where(commit_id: commit).first 
        c.deployable_entity.should eq(d)
        d.commits.should eq([c])
      end
    end

    it "should not add a version that already exists" do
      Gh.should_receive(:add_message_and_author)

      VCR.use_cassette('entity-save-erlattest', allow_playback_repeats: true) do
        d = DeployableEntity.new valid_params
        d.save.should eq(true)

        # Fine, now we have a valid entity
        commit = "commit_id"
        d.add_commit commit
        d.deployable_entity_versions.reload.count.should eq 1

        # Should not re-add a commit if it exists
        d.add_commit commit
        d.deployable_entity_versions.reload.count.should eq 1
      end
    end

    it "should know the status of the last build" do
      de = PreRecorded.setup_deployable_entity
 
      # There is no version yet
      de.status.should eq ""

      dev = PreRecorded.setup_deployable_entity_version de
      de.status.should eq dev.status
    end
  end
end
