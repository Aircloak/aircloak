require 'spec_helper'
require './lib/protobuf_sender'
require './lib/proto/air/management_messages.pb.rb'

describe Cluster do
  before(:each) do
    ProtobufSender.stub(:post)
    ProtobufSender.stub(:send_delete)
    ClusterCloak.destroy_all
    Cluster.delete_all
    Cloak.destroy_all
    Build.delete_all
    BuildManager.stub(:send_build_request)
  end

  let (:cloak) { Cloak.create(name: "dave", ip: "9.9.9.9") }
  let (:richard) { Cloak.create(name: "richard", ip: "10.10.10.10") }
  let (:build) { Build.create(name: "build") }

  def base_cluster vals={}
    Cluster.new(
      name: vals.delete(:name) || "test",
      build: vals.delete(:build) || build
    )
  end

  it "should have a build" do
    c = Cluster.new(name: "test")
    c.cloaks << cloak
    c.save.should eq false
    c.errors.messages[:build].should_not eq nil
  end

  it "should have a name" do
    c = Cluster.new(build: build)
    c.cloaks << cloak
    c.save.should eq false
    c.errors.messages[:name].should_not eq nil
  end

  it "should have at least a cloak" do
    c = base_cluster
    c.save.should eq false
    c.errors.messages[:cloaks].should_not eq nil
  end

  it "should require a unique name" do
    c1 = base_cluster
    c1.cloaks << cloak
    c1.save.should eq true

    c2 = base_cluster
    c2.cloaks << richard
    c2.save.should eq false
    c2.errors.messages[:name].should_not eq nil
  end

  it "should only allow identical cloaks for a build" do
    build.save.should eq true

    c1 = Cloak.new(name: "cloak2", ip: "20.20.20.20")
    c1.tpm = true
    c1.save.should eq true

    cl = base_cluster name: "cluster1", build: build
    cl.assign_cloaks([c1]).should eq true

    c2 = Cloak.new(name: "cloak1", ip: "10.10.10.10")
    c2.tpm = false
    c2.save.should eq true

    cl.assign_cloaks([c1, c2]).should eq false
    cl.errors.messages[:cloaks].should_not eq nil
  end

  it "should know if a cluster is healthy" do
    cluster = base_cluster
    c = Cloak.new
    cluster.cloaks << c

    cluster.health.should eq :healthy

    c.good = false
    cluster.health.should eq :poor
  end

  context "#assign_cloaks" do
    let! (:cloak1) { Cloak.create(name: "foo", ip: "1.1.1.1") }
    let! (:cloak2) { Cloak.create(name: "bar", ip: "2.2.2.2") }
    let! (:cloak3) { Cloak.create(name: "baz", ip: "3.3.3.3") }
    let! (:build) { Build.create(name: "build") }
    let! (:cluster) { base_cluster name: "cluster", build: build }

    it "should add all new selected cloaks as :to_be_added" do
      cluster.assign_cloaks [cloak1, cloak2]
      cluster.save.should eq true
      cloak1.reload.cluster_cloak.cluster.should eq cluster
      cloak1.cluster_cloak.state.should eq :to_be_added
      cloak2.reload.cluster_cloak.cluster.should eq cluster
      cloak2.cluster_cloak.state.should eq :to_be_added
      cloak3.reload.cluster_cloak.should eq nil
      cluster.assign_cloaks [cloak1, cloak2, cloak3]
      cloak1.reload.cluster_cloak.cluster.should eq cluster
      cloak1.cluster_cloak.state.should eq :to_be_added
      cloak2.reload.cluster_cloak.cluster.should eq cluster
      cloak2.cluster_cloak.state.should eq :to_be_added
      cloak3.reload.cluster_cloak.cluster.should eq cluster
      cloak3.cluster_cloak.state.should eq :to_be_added
    end

    it "should remove all no-longer selected cloaks" do
      cluster.assign_cloaks [cloak1, cloak2, cloak3]
      cluster.save.should eq true
      cluster.reload.assign_cloaks [cloak2, cloak3]
      cloak1.reload.cluster_cloak.should_not eq nil
      cloak1.cluster_cloak.state.should eq :to_be_removed
    end

    it "should keep all selected cloaks with the old state" do
      cluster.assign_cloaks [cloak1, cloak2]
      cluster.save.should eq true
      cloak2.reload.cluster_cloak.set_state :belongs_to
      cloak2.cluster_cloak.save.should eq true
      cluster.assign_cloaks [cloak1, cloak2, cloak3]
      cluster.save.should eq true
      cloak1.reload.cluster_cloak.cluster.should eq cluster
      cloak1.cluster_cloak.state.should eq :to_be_added
      cloak2.reload.cluster_cloak.cluster.should eq cluster
      cloak2.cluster_cloak.state.should eq :belongs_to
    end

    it "should mark all :to_be_removed cloaks as :to_be_added when selected" do
      cluster.assign_cloaks [cloak1, cloak2]
      cluster.save.should eq true
      cluster.reload.assign_cloaks [cloak2]
      cluster.save.should eq true
      cloak1.reload.cluster_cloak.state.should eq :to_be_removed
      cluster.assign_cloaks [cloak1, cloak2]
      cluster.save.should eq true
      cloak1.reload.cluster_cloak.state.should eq :to_be_added
    end
  end

  context "testing" do
    let (:cloak1) { Cloak.create(name: "foo", ip: "1.1.1.1", tpm: false) }
    let (:cloak2) { Cloak.create(name: "bar", ip: "2.2.2.2", tpm: false) }
    let (:cloak3) { Cloak.create(name: "baz", ip: "3.3.3.3", tpm: false) }
    let (:cloak_tpm) { Cloak.create(name: "tpm", ip: "4.4.4.4.4", tpm: true) }
    let (:build) { Build.create(name: "build") }

    it "should create a test cluster for a build" do
      cloak1; cloak2; cloak3 # Create the cloaks
      c = Cluster.test_cluster_for_build build
      c.build.should be build
    end

    it "should raise an exception if there aren't sufficient cloaks for testing" do
      expect{Cluster.test_cluster_for_build build}.to raise_exception(NotEnoughCloaks)
    end

    it "should not notify the version test, before all cloaks are ready" do
      cloak1; cloak2; cloak3;
      c = Cluster.test_cluster_for_build build
      c.version_test = VersionTest.create
      ProtobufSender.should_not_receive(:post_to_url)
      c.cluster_cloaks.first.set_state :belongs_to
    end

    it "should notify the version test, when all cloaks are ready" do
      cloaks = [cloak1, cloak2, cloak3]
      c = Cluster.test_cluster_for_build build
      c.version_test = VersionTest.new
      ProtobufSender.should_receive(:post_to_url)
      c.cluster_cloaks.each do |cc|
        cc.set_state :belongs_to
      end
    end
  end

  context "should know if it can be deleted" do
    let (:cluster) { Cluster.create }
    it "should know it cannot be delete if used by a test or has cloaks" do
      c = base_cluster
      c.can_destroy?.should eq true

      c.cloaks << cloak
      c.save.should eq true

      c.cloaks = []
      c.can_destroy?.should eq true

      c.version_test = VersionTest.new
      c.can_destroy?.should eq false

      c.destroy.should eq false
      c.destroyed?.should eq false
    end
  end

  def cluster args={}
    b = Build.new name: args.delete(:bname) || "Build name"
    Cluster.new name: args.delete(:cname) || "Cluster name", build: b
  end

  it "should set and get status values" do
    c = cluster
    c.status = :active
    c.status.should eq :active
    c.status = :in_service
    c.status.should eq :in_service
    c.status = :inactive
    c.status.should eq :inactive
  end

  it "should clear status description when active" do
    c = cluster
    c.status = :inactive
    msg = "Failed for some reason"
    c.status_description = msg
    c.status_description.should eq msg
    c.status = :active
    c.status_description.should eq ""
  end

  it "should have a status for display" do
    c = cluster
    c.status_description = "Description"
    c.status = :active
    c.status_for_display.should == "Active"
    c.status_description = "Description"
    c.status = :in_service
    c.status_for_display.should == "In service: Description"
    c.status_description = "Description"
    c.status = :inactive
    c.status_for_display.should == "Inactive: Description"
  end

  it "should truncate status descriptions that are longer than 255" do
    c = cluster
    long_description = %{This is a description that unfortunately
        is longer than what is allowed for a text column in Postgresql.
        The text columns only accept 255 characters, so the system will
        barf if the controller does not handle it gracefully!
        It should shorten the description appropriately, and add
        '...' to the end if it is too long}
    c.status_description = long_description
    c.status_description.size.should eq 255
    c.status_description[-3..-1].should eq "..."
  end
end
