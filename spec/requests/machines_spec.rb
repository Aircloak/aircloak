require 'spec_helper'
require './lib/proto/air/management_messages.pb'
require './lib/machine_packer.rb'

describe "ResultsController" do
  describe "GET /machines" do
    it "should provide a list of machines" do
      machine = MachineProto.new(
        machine_id: 1,
        name: "tpm-monster.mpi-sws.org",
        type: MachineProto::MachineType::PHYSICAL
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
end
