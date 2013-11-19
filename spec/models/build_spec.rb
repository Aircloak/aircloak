require 'spec_helper'

describe Build do
  before(:each) do
    BuildManager.stub(:send_build_request).and_return(true)
    Cluster.destroy_all
    Build.destroy_all
    BuildVersion.destroy_all
    DeployableEntity.destroy_all
    DeployableEntityVersion.destroy_all
  end

  it "should have a name" do
    b = Build.new
    b.save.should eq false
    b.errors.messages[:name].should_not eq nil
  end

  it "should require a unique name" do
    b1 = Build.new name: "test"
    b1.save.should eq true

    b2 = Build.new name: "test"
    b2.save.should eq false
    b2.errors.messages[:name].should_not eq nil
  end

  it "should have versions through build_versions" do
    deployable_entity = PreRecorded.setup_deployable_entity
    deployable_entity_version = PreRecorded.setup_deployable_entity_version deployable_entity

    b = Build.create name: "test"
    b.deployable_entity_versions << deployable_entity_version
    b.save
    BuildVersion.first.deployable_entity_version_id.should eq deployable_entity_version.id
    BuildVersion.first.build_id.should eq b.id
  end

  it "should be able to tell the id of entity versions it is using" do
    deployable_entity = PreRecorded.setup_deployable_entity
    deployable_entity_version = PreRecorded.setup_deployable_entity_version deployable_entity

    b = Build.create name: "test"
    # We default to 0 (since that isn't going to match any select element
    # in the form) if there have not been assigned any entity version
    # for an entity
    b.version_for_entity(deployable_entity).should eq(0)

    b.deployable_entity_versions << deployable_entity_version
    b.version_for_entity(deployable_entity).should eq(deployable_entity_version.id)
  end

  it "should have a fingerprint after save" do
    deployable_entity = PreRecorded.setup_deployable_entity
    deployable_entity_version = PreRecorded.setup_deployable_entity_version deployable_entity

    b = Build.new name: "test"
    b.deployable_entity_versions << deployable_entity_version
    b.save

    b.fingerprint.should eq FingerPrintCreator.fingerprint b
  end

  it "should not save two builds with the same versions" do
    Build.create name: "test"
    b = Build.new name: "test2"
    b.save.should eq false
    b.errors.messages[:fingerprint].should_not eq nil
  end

  context "should know if it can be deleted" do
    let(:build) { Build.create name: "test-build" }
    let(:cluster) { Cluster.create name: "test-cluster", build: build }

    it "should say it can be deleted if not part of a cluster" do
      build.can_destroy?.should eq true
    end

    it "should say it cannot be destroyed if it is part of a cluster" do
      cluster.build.can_destroy?.should eq false
    end

    it "should not be possible to delete a build that has a cluster" do
      cluster.build.destroy
      cluster.build.destroyed?.should eq false
    end
  end
end
