require 'spec_helper'
require './lib/protobuf_sender'

describe Build do
  before(:each) do
    VersionTest.stub(:new_from_deployable_entity_version)
    ProtobufSender.stub(:post)
    Net::HTTP.stub(:delete)
    BuildManager.stub(:send_build_request).and_return(true)
    ClusterCloak.destroy_all
    Cloak.destroy_all
    Cluster.destroy_all
    Build.destroy_all
    BuildVersion.destroy_all
    DeployableEntity.destroy_all
    DeployableEntityVersion.destroy_all
    OsTag.destroy_all
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
    let(:cloak) { Cloak.create name: "cloak", ip: "1.1.1.1" }
    let(:os_tag) { OsTag.create name: "Fab", description: "Best tag ever" }
    let(:cluster) { Cluster.create name: "test-cluster", build: build, cloaks: [cloak], os_tag: os_tag }

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

  it "should mark itself as done when mark_complete is called" do
    b = Build.create(name: "test name")
    expect{b.mark_complete}.to change{b.build_completed}.from(nil).to(true)
  end

  it "should retain success status from mark_complete call" do
    b = Build.create(name: "test name")
    expect{b.mark_complete success: true}.to change{b.build_success}.from(nil).to(true)
    expect{b.mark_complete success: false}.to change{b.build_success}.from(true).to(false)
  end

  context "testing" do
    it "should notify the version test, if it has one, when the build has completed" do
      b = Build.new
      v = VersionTest.new
      b.version_test = v
      v.should_receive(:mark_build_as_complete)
      b.mark_complete success: true
    end

    it "should invalidate a version test if a build fails" do
      b = Build.new
      v = VersionTest.new
      b.version_test = v
      v.should_receive(:mark_build_as_failed)
      b.mark_complete success: false
    end
  end
end
