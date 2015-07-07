require 'spec_helper'
require './lib/protobuf_sender'

describe Cluster do
  before(:each) do
    ProtobufSender.stub(:post)
    ProtobufSender.stub(:send_delete)
    ClusterCloak.destroy_all
    Cluster.destroy_all
    Cloak.destroy_all
    Build.destroy_all
    BuildManager.stub(:send_build_request)
    AuditLog.delete_all
  end

  let! (:cloak) { Cloak.create(name: "dave", ip: "9.9.9.9") }
  let! (:build) { Build.create(name: "build") }
  let! (:cluster) { Cluster.create(name: "cluster", build: build, cloaks: [cloak]) }

  it "should go from :to_be_added to :belongs_to if synchronized" do
    cloak.reload.cluster_cloak.synchronize
    cloak.cluster_cloak.state.should eq :belongs_to
  end

  it "should get destroyed if :to_be_removed and synchronized" do
    cloak.reload.cluster_cloak.set_state :to_be_removed
    cloak.cluster_cloak.synchronize
    cloak.reload.cluster_cloak.should eq nil
  end

  it "should destroy audit logs when destroyed" do
    cloak.reload.cluster_cloak.set_state :to_be_removed
    msg = ";HASH;146;hello;2014-12-19T15:40:05Z;world"
    AuditLog.create_from_log_message(msg, cloak.id).save
    AuditLog.count.should eq 1
    cloak.cluster_cloak.synchronize
    AuditLog.count.should eq 0
  end

  it "cluster should get removed if last cloak is :to_be_removed and gets synchronized" do
    cloak.reload.cluster_cloak.set_state :to_be_removed
    cloak.cluster_cloak.synchronize
    Cluster.where(id: cluster.id).size.should eq 0
  end
end
