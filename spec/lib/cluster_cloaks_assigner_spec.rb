require 'spec_helper'

require './lib/cluster_cloaks_assigner.rb'

describe ClusterCloaksAssigner do
  context "self.get_cloaks" do
    before(:each) do
      Cloak.destroy_all
    end

    it "should return list of all selected cloaks" do
      Cloak.create!(name: "foo", ip: "1.1.1.1")
      cloak2 = Cloak.create!(name: "bar", ip: "2.2.2.2")
      cloak3 = Cloak.create!(name: "baz", ip: "3.3.3.3")
      Cloak.create!(name: "xxx", ip: "4.4.4.4")
      params = {"cloak_selections" => [cloak2.id.to_s, cloak3.id.to_s]}

      ClusterCloaksAssigner.get_cloaks(params).should == [cloak2, cloak3]
    end
  end

  context "self.assign_from_list" do
    before(:each) do
      Cloak.destroy_all
      Cluster.destroy_all
      ClusterCloak.destroy_all
      Build.destroy_all
      BuildManager.stub(:send_build_request)
    end

    let! (:cloak1) { Cloak.create!(name: "foo", ip: "1.1.1.1") }
    let! (:cloak2) { Cloak.create!(name: "bar", ip: "2.2.2.2") }
    let! (:cloak3) { Cloak.create!(name: "baz", ip: "3.3.3.3") }
    let! (:build) { Build.create(name: "build") }
    let! (:cluster) { Cluster.create(name: "cluster", tpm: true, build: build) }

    it "should add all new selected cloaks" do
      ClusterCloaksAssigner.assign_from_list cluster, [cloak1, cloak2]
      cloak1.reload.cluster_cloak.cluster.should eq cluster
      cloak2.reload.cluster_cloak.cluster.should eq cluster
      cloak3.reload.cluster_cloak.should eq nil
      ClusterCloaksAssigner.assign_from_list cluster, [cloak1, cloak2, cloak3]
      cloak1.reload.cluster_cloak.cluster.should eq cluster
      cloak2.reload.cluster_cloak.cluster.should eq cluster
      cloak3.reload.cluster_cloak.cluster.should eq cluster
    end

    it "should remove all no-longer selected cloaks" do
      ClusterCloaksAssigner.assign_from_list cluster, [cloak1, cloak2, cloak3]
      ClusterCloaksAssigner.assign_from_list cluster, [cloak2, cloak3]
      cloak1.reload.cluster_cloak.should eq nil
    end

    it "should keep all selected cloaks" do
      ClusterCloaksAssigner.assign_from_list cluster, [cloak1, cloak2]
      ClusterCloaksAssigner.assign_from_list cluster, [cloak2, cloak3]
      cloak2.reload.cluster_cloak.cluster.should eq cluster
    end
  end
end
