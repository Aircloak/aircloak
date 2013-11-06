require 'spec_helper'

describe Build do
  before(:each) do
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
end
