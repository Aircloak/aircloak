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
      Net::HTTP.stub(:delete)
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
      Net::HTTP.stub(:delete)
      Cloak.destroy_all
      post broken_machine_path(1)
      response.status.should be(404)
    end
  end

  describe "POST /api/machines/:id/synchronize" do
    before(:each) do
      ProtobufSender.stub(:post)
      Net::HTTP.stub(:delete)
      ClusterCloak.destroy_all
      Cluster.destroy_all
      Cloak.destroy_all
      Build.destroy_all
      BuildManager.stub(:send_build_request)
    end

    let! (:cloak) { Cloak.create(name: "dave", ip: "9.9.9.9") }
    let! (:build) { Build.create(name: "build") }
    let! (:cluster) { Cluster.create(name: "cluster", build: build, cloaks: [cloak]) }

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
end
