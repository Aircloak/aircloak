require 'spec_helper'

describe MachinesController do
  describe "UPDATE /machines/MACHINEID" do
    it "should update the health of MACHINEID" do
      machine_state = MachineStateProto.new(state: MachineStateProto::State::GOOD)
      Cloak.destroy_all
      cloak = Cloak.create(name: "cloak", ip: "1.1.1.1")

      request.env['RAW_POST_DATA'] = machine_state.encode.buf
      post :update, id: cloak.id
      
      response.status.should be(200)
      cloak.reload.health.should == :good
    end
  end
end
