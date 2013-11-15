require 'spec_helper'

describe Cloak do
  context "setting and getting health" do
    let(:cloak) { Cloak.new }

    it "should know what health types are supported" do
      Cloak.health_types.should eq [:good, :changing, :sw_failing, :hw_failing, :unknown]
    end

    it "should default to an unknown health" do
      cloak.health.should eq :unknown
    end

    it "should have the ability to get and set health" do
      Cloak.health_types.each do |health|
        cloak.set_health health
        cloak.health.should eq health
      end
    end
  end

  context "validations" do
    before(:each) do
      Cloak.destroy_all
    end

    it "should have a valid raw_health" do
      cloak = Cloak.new(name: "cloak", ip: "1.1.1.1", raw_health: -1)
      cloak.save.should eq false
      cloak.errors.messages[:raw_health].should_not eq nil

      cloak = Cloak.new(name: "cloak", ip: "1.1.1.1", raw_health: 5)
      cloak.save.should eq false
      cloak.errors.messages[:raw_health].should_not eq nil

      cloak = Cloak.new(name: "cloak", ip: "1.1.1.1", raw_health: 0)
      cloak.save.should eq true
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

  context "self.all_unassigned" do
    before(:each) do
      Cloak.destroy_all
      Build.destroy_all
      Cluster.destroy_all
      ClusterCloak.destroy_all
      BuildManager.stub(:send_build_request)
    end

    let! (:cloak1) { Cloak.create(name: "foo", ip: "1.1.1.1", raw_health: 0) }
    let! (:cloak2) { Cloak.create(name: "bar", ip: "2.2.2.2", raw_health: 0) }
    let! (:cloak3) { Cloak.create(name: "baz", ip: "3.3.3.3", raw_health: 0) }
    let! (:build) { Build.create(name: "build") }
    let! (:cluster) { Cluster.create(name: "cluster", build: build) }

    it "should return all unassigned cloaks" do
      cluster.cloaks << cloak1
      cluster.cloaks << cloak2
      Cloak.all_unassigned.should eq [cloak3]
    end
  end
end
