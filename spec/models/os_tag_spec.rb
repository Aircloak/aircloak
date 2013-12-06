require 'spec_helper'
require './lib/protobuf_sender'
require './lib/build_manager'

describe OsTag do
  before(:each) do
    ProtobufSender.stub(:post)
    ProtobufSender.stub(:send_delete)
    BuildManager.stub(:send_build_request)
    ClusterCloak.destroy_all
    Cluster.destroy_all
    Cloak.destroy_all
    Build.destroy_all
    OsTag.destroy_all
  end

  it "should have a name" do
    OsTag.create.errors.should include(:name)
  end

  it "should have a description" do
    OsTag.create.errors.should include(:description)
  end

  let (:os_tag) { OsTag.new name: "Name", description: "Description" }
  let (:build) { Build.new tpm: true, name: "Build name" }
  let (:cloak) { Cloak.create(name: "cloak", ip: "9.9.9.9") }
  let (:cluster) { Cluster.new name: "ClusterName", os_tag: os_tag, build: build, cloaks: [cloak] }

  it "should know if it can be destroyed" do
    os_tag.can_destroy?.should eq true
    cluster.save
    cluster.os_tag.can_destroy?.should eq false
  end

  it "should not be possible to delete if it cannot be destroyed" do
    cluster.save
    os_tag.destroy.should eq false
    cluster.cloaks = []
    cluster.destroy
    os_tag.destroy.should_not eq false
    os_tag.destroyed?.should eq true
  end
end
