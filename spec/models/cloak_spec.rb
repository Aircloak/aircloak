require 'spec_helper'
require './lib/protobuf_sender'

describe Cloak do
  context "basic validations" do
    before(:each) do
      ProtobufSender.stub(:post)
      Net::HTTP.stub(:delete)
    end

    context "validations" do
      before(:each) do
        # remove cluster-cloaks as we cannot remove cloaks if they are assigned to a cluster
        ClusterCloak.destroy_all
        Cloak.destroy_all
      end

      it "should have a valid health" do
        cloak = Cloak.create(name: "cloak", ip: "1.1.1.1")
        cloak.good.should_not eq nil
      end

      it "should have a name" do
        cloak = Cloak.new
        cloak.save.should eq false
        cloak.errors.messages[:name].should_not eq nil
      end

      it "should have an ip" do
        cloak = Cloak.new(name: "cloak")
        cloak.save.should eq false
        cloak.errors.messages[:ip].should_not eq nil
      end

      it "should have an unique name" do
        cloak1 = Cloak.create(name: "cloak", ip: "1.1.1.1")
        cloak2 = Cloak.new(name: "cloak", ip: "2.2.2.2")
        cloak2.save.should eq false
        cloak2.errors.messages[:name].should_not eq nil
      end

      it "should have an unique ip" do
        cloak1 = Cloak.create(name: "cloak1", ip: "1.1.1.1")
        cloak2 = Cloak.new(name: "cloak2", ip: "1.1.1.1")
        cloak2.save.should eq false
        cloak2.errors.messages[:ip].should_not eq nil
      end
    end

    context "cloak cluster interactions" do
      before(:each) do
        ClusterCloak.destroy_all
        Cluster.destroy_all
        Cloak.destroy_all
        Build.destroy_all
        BuildManager.stub(:send_build_request)
      end

      let! (:cloak1) { Cloak.create(name: "foo", ip: "1.1.1.1") }
      let! (:cloak2) { Cloak.create(name: "bar", ip: "2.2.2.2") }
      let! (:cloak3) { Cloak.create(name: "baz", ip: "3.3.3.3") }
      let! (:build) { Build.create(name: "build") }
      let! (:cluster) { Cluster.create(name: "cluster", build: build) }

      it "should return all unassigned cloaks" do
        cluster.cloaks << cloak1
        cluster.cloaks << cloak2
        Cloak.all_unassigned.should eq [cloak3]
      end

      it "should destroy a cloak not belonging to a cluster" do
        cluster.cloaks << cloak1
        cluster.cloaks << cloak2
        cloak3.can_destroy?.should eq true
        cloak3.destroy.destroyed?.should eq true
      end

      it "should not destroy a cloak belonging to a cluster" do
        cluster.cloaks << cloak1
        cloak1.can_destroy?.should eq false
        cloak1.destroy.should eq false
      end
    end
  end

  context "connection to manny-air" do
    before(:each) do
      Net::HTTP.stub(:delete)
      ClusterCloak.destroy_all
      Cluster.destroy_all
      Cloak.destroy_all
      Build.destroy_all
      BuildManager.stub(:send_build_request)
    end

    it "should inform about new cloaks" do
      ProtobufSender.should_receive(:post_to_url) { |url, pb| pb.name == "cloak" }
      Cloak.create(name: "cloak", ip: "1.1.1.1")
    end

    it "should inform about removed cloaks" do
      ProtobufSender.stub(:post)
      cloak = Cloak.create(name: "cloak", ip: "1.1.1.1")
      Net::HTTP.should_receive(:delete).with("http://manny-air.aircloak.com/machines/#{cloak.id}")
      cloak.destroy
    end
  end
end
