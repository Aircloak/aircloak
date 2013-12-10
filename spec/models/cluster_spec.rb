require 'spec_helper'
require './lib/protobuf_sender'

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
  let (:os_tag) { OsTag.create(name: "First version", description: "Crucial OS stuff") }

  def base_cluster vals={}
    Cluster.new(
      name: vals.delete(:name) || "test",
      build: vals.delete(:build) || build, 
      os_tag: vals.delete(:os_tag) || os_tag
    )
  end

  it "should have a build" do
    c = Cluster.new(name: "test")
    c.cloaks << cloak
    c.save.should eq false
    c.errors.messages[:build].should_not eq nil
  end

  it "should have a os_tag" do
    c = Cluster.new(name: "test", build: build)
    c.cloaks << cloak
    c.save.should eq false
    c.errors.messages[:os_tag].should_not eq nil
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

  it "cloak and cluster tpm settings should match" do
    build.tpm = false
    build.save.should eq true

    cl1 = Cloak.new(name: "cloak1", ip: "10.10.10.10")
    cl1.tpm = false
    cl1.save.should eq true

    cl2 = Cloak.new(name: "cloak2", ip: "20.20.20.20")
    cl2.tpm = true
    cl2.save.should eq true

    c1 = base_cluster name: "cluster1", build: build
    c1.cloaks << cl1
    c1.save.should eq true

    c2 = base_cluster name: "cluster2", build: build
    c2.cloaks << cl2
    c2.save.should eq false
    c2.errors.messages[:cloaks].should_not eq nil
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

  context "connection to manny-air" do
    before(:each) do
      ProtobufSender.stub(:send_delete)
      ClusterCloak.destroy_all
      Cluster.destroy_all
      Cloak.destroy_all
      Build.destroy_all
      BuildManager.stub(:send_build_request)
    end

    let! (:build) { Build.create(name: "build") }
    let! (:cluster) { base_cluster name: "cluster", build: build }

    it "should inform about new clusters" do
      cluster.cloaks << cloak
      ProtobufSender.should_receive(:post_to_url) { |url, pb| pb.cluster_id == cluster.id }
      cluster.save.should eq true
    end

    it "should inform about cluster changes if cloaks are added" do
      cluster.cloaks << cloak
      cluster.save.should eq true
      cluster.cloaks << richard
      ProtobufSender.should_receive(:post_to_url) { |url, pb| pb.cluster_id == cluster.id }
      cluster.save.should eq true
    end

    it "should inform about cluster changes if cloaks are removed" do
      cluster.cloaks << cloak
      cluster.cloaks << richard
      cluster.save.should eq true
      ProtobufSender.should_receive(:post_to_url) { |url, pb| pb.cluster_id == cluster.id }
      cluster.assign_cloaks [cloak]
      cluster.save.should eq true
    end
  end

  context "testing" do
    let (:cloak1) { Cloak.create(name: "foo", ip: "1.1.1.1", tpm: false) }
    let (:cloak2) { Cloak.create(name: "bar", ip: "2.2.2.2", tpm: false) }
    let (:cloak3) { Cloak.create(name: "baz", ip: "3.3.3.3", tpm: false) }
    let (:cloak_tpm) { Cloak.create(name: "tpm", ip: "4.4.4.4.4", tpm: true) }
    let (:build) { Build.create(name: "build", tpm: false) }

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
end
