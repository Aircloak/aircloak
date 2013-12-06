require 'spec_helper'
require './lib/proto/air/management_messages.pb'
require './lib/machine_packer.rb'
require './lib/protobuf_sender'

describe MachinesController do
  describe "GET /api/machines" do
    it "should provide a list of machines" do
      machine = MachineProto.new(
        machine_id: 1,
        name: "tpm-monster.mpi-sws.org",
        type: MachineProto::MachineType::PHYSICAL,
        good: true
      )
      p = MachinesProto.new machines: [machine]

      m = double
      Cloak.stub(:all).and_return [m]
      MachinePacker.stub(:package_cloaks).with([m]).and_return(p)

      get machines_path

      resp = MachinesProto.decode(response.body)
      resp.should eq p
      response.status.should be(200)
    end
  end

  describe "POST /api/machines/:id/broken" do
    it "should mark a machine as brkoen" do
      ProtobufSender.stub(:post)
      ProtobufSender.stub(:send_delete)
      ClusterCloak.destroy_all
      Cloak.destroy_all
      cloak = Cloak.create(name: "cloak", ip: "1.1.1.1")
      cloak.good.should eq true
      post broken_machine_path(cloak.id)
      response.status.should be(200)
      cloak.reload.good.should eq false
    end

    it "should return with an error on unknown machines" do
      ProtobufSender.stub(:post)
      ProtobufSender.stub(:send_delete)
      Cloak.destroy_all
      post broken_machine_path(1)
      response.status.should be(404)
    end
  end

  describe "POST /api/machines/:id/synchronize" do
    before(:each) do
      ProtobufSender.stub(:post)
      ProtobufSender.stub(:send_delete)
      ClusterCloak.destroy_all
      Cluster.destroy_all
      Cloak.destroy_all
      Build.destroy_all
      BuildManager.stub(:send_build_request)
      OsTag.destroy_all
    end

    let! (:cloak) { Cloak.create(name: "dave", ip: "9.9.9.9") }
    let! (:build) { Build.create(name: "build") }
    let (:os_tag) { OsTag.create(name: "OsTag", description: "Smartness") }
    let! (:cluster) { Cluster.create(name: "cluster", build: build, cloaks: [cloak], os_tag: os_tag) }

    it "should synchronize a machine belonging to a cluster" do
      post synchronize_machine_path(cloak.id)
      response.status.should be(200)
      cloak.reload.cluster_cloak.reload.state.should eq :belongs_to
    end

    it "should return with an error on unknown machines" do
      post synchronize_machine_path(cloak.id.to_i + 1)
      response.status.should be(404)
    end

    it "should return with an error on machines not in a cluster" do
      cloak2 = Cloak.create(name: "dave2", ip: "8.8.8.8")
      post synchronize_machine_path(cloak2.id)
      response.status.should be(400)
    end
  end

  describe "GET /api/machines/setup_info" do
    before(:each) do
      Cloak.delete_all
    end

    it "should return an error if called from a machine which isn't known" do
      Cloak.count.should eq 0
      get setup_info_machines_path
      response.status.should eq(400)
    end

    it "should return a file containing the build id and the os tag if the machine is part of a cluster" do
      build = double(id: 14)
      os_tag = double(name: "tag_name")
      cluster = double(build: build, os_tag: os_tag)
      cloak = double(cluster: cluster)
      Cloak.stub(:find_by_ip).and_return(cloak)
      get setup_info_machines_path
      response.status.should be(200)
      response.body.should include("perform_aircloak_install=true")
      response.body.should include("14")
      response.body.should include("tag_name")
    end

    it "should return a file with a flag signifying that a aircloak specific build should not be performed for machines without clusters" do
      cloak = double(cluster: nil)
      Cloak.stub(:find_by_ip).and_return(cloak)
      most_recent_tag = double(name: "tag_name")
      OsTag.should_receive(:last).and_return(most_recent_tag)
      get setup_info_machines_path
      response.status.should be(200)
      response.body.should include("base_image_tag=\"tag_name\"")
      response.body.should include("perform_aircloak_install=false")
    end
  end
end
