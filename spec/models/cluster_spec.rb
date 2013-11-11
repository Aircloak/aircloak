require 'spec_helper'

describe Cluster do
  before(:each) do
    Cloak.destroy_all
    Build.destroy_all
    Cluster.destroy_all
    BuildManager.stub(:send_build_request)
  end

  it "should have a name" do
    c = Cluster.new
    c.save.should eq false
    c.errors.messages[:name].should_not eq nil
  end

  it "should have a build" do
    c = Cluster.new name: "test"
    c.save.should eq false
    c.errors.messages[:build_id].should_not eq nil
  end

  it "should require a unique name" do
    b = Build.new name: "build"
    b.save.should eq true

    c1 = Cluster.new(name: "test", build: b)
    c1.save.should eq true

    c2 = Cluster.new(name: "test", build: b)
    c2.save.should eq false
    c2.errors.messages[:name].should_not eq nil
  end

  it "build and cluster tpm settings should match" do
    b1 = Build.new name: "build1"
    b1.tpm = false
    b1.save.should eq true

    b2 = Build.new name: "build2"
    b2.tpm = true
    b2.save.should eq true

    c1 = Cluster.new(name: "cluster1", build: b1)
    c1.tpm = false
    c1.save.should eq true

    c2 = Cluster.new(name: "cluster2", build: b2)
    c2.tpm = false
    c2.save.should eq false
    c2.errors.messages[:build].should_not eq nil
  end

  it "cloak and cluster tpm settings should match" do
    b = Build.new name: "build"
    b.tpm = false
    b.save.should eq true

    cl1 = Cloak.new(name: "cloak1", ip: "10.10.10.10")
    cl1.tpm = false
    cl1.save.should eq true

    cl2 = Cloak.new(name: "cloak2", ip: "20.20.20.20")
    cl2.tpm = true
    cl2.save.should eq true

    c1 = Cluster.new(name: "cluster1", build: b)
    c1.cloaks << cl1
    c1.tpm = false
    c1.save.should eq true

    c2 = Cluster.new(name: "cluster2", build: b)
    c2.cloaks << cl2
    c2.tpm = false
    c2.save.should eq false
    c2.errors.messages[:cloaks].should_not eq nil
  end
end
