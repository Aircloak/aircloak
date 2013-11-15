require 'spec_helper'

describe Cluster do
  before(:each) do
    Cloak.destroy_all
    Build.destroy_all
    Cluster.destroy_all
    ClusterCloak.destroy_all
    BuildManager.stub(:send_build_request)
  end

  it "should have a build" do
    c = Cluster.new name: "test"
    c.save.should eq false
    c.errors.messages[:build].should_not eq nil
  end

  it "should have a name" do
    b = Build.new name: "build"
    b.save.should eq true

    c = Cluster.new build: b
    c.save.should eq false
    c.errors.messages[:name].should_not eq nil
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
    c1.save.should eq true

    c2 = Cluster.new(name: "cluster2", build: b)
    c2.cloaks << cl2
    c2.save.should eq false
    c2.errors.messages[:cloaks].should_not eq nil
  end

  it "should be able to list the health of it's cloaks" do
    cluster = Cluster.new
    c1 = Cloak.new
    cluster.cloaks << c1
    c1.set_health :good

    types = Cloak.health_types - [:good]
    types.each do |type|
      cluster.health_of_cloaks[type].should eq 0
    end
    cluster.health_of_cloaks[:good].should eq 1
  end

  context "#assign_cloaks" do
    let! (:cloak1) { Cloak.create(name: "foo", ip: "1.1.1.1") }
    let! (:cloak2) { Cloak.create(name: "bar", ip: "2.2.2.2") }
    let! (:cloak3) { Cloak.create(name: "baz", ip: "3.3.3.3") }
    let! (:build) { Build.create(name: "build") }
    let! (:cluster) { Cluster.create(name: "cluster", build: build) }

    it "should add all new selected cloaks" do
      cluster.assign_cloaks [cloak1, cloak2]
      cloak1.reload.cluster_cloak.cluster.should eq cluster
      cloak2.reload.cluster_cloak.cluster.should eq cluster
      cloak3.reload.cluster_cloak.should eq nil
      cluster.assign_cloaks [cloak1, cloak2, cloak3]
      cloak1.reload.cluster_cloak.cluster.should eq cluster
      cloak2.reload.cluster_cloak.cluster.should eq cluster
      cloak3.reload.cluster_cloak.cluster.should eq cluster
    end

    it "should remove all no-longer selected cloaks" do
      cluster.assign_cloaks [cloak1, cloak2, cloak3]
      cluster.assign_cloaks [cloak2, cloak3]
      cloak1.reload.cluster_cloak.should eq nil
    end

    it "should keep all selected cloaks" do
      cluster.assign_cloaks [cloak1, cloak2]
      cluster.assign_cloaks [cloak2, cloak3]
      cloak2.reload.cluster_cloak.cluster.should eq cluster
    end
  end
end
