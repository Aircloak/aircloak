require 'spec_helper'

describe VersionTest do
  before(:each) do
    DeployableEntity.destroy_all
    DeployableEntityVersion.destroy_all
    VersionTest.destroy_all
  end

  let(:entity) { PreRecorded.setup_deployable_entity }
  let(:entity_version) { PreRecorded.setup_deployable_entity_version entity }

  it "should create a new VersionTest and assign it to the version" do
    entity_version.version_test.deployable_entity_version.id.should be entity_version.id
  end
end
