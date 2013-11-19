require 'spec_helper'
require './lib/proto/air/management_messages.pb'
require './lib/machine_packer.rb'
require './lib/protobuf_sender'

describe MachinesController do
  describe "GET /machines" do
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

      get "/machines"

      resp = MachinesProto.decode(response.body)
      resp.should eq p
      response.status.should be(200)
    end
  end

  describe "POST /machines/:id/broken" do
    it "should mark a machine as brkoen" do
      ProtobufSender.stub(:post)
      Net::HTTP.stub(:delete)
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
end
