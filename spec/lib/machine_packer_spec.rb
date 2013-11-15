require './lib/machine_packer.rb'

describe MachinePacker do
  context "managing state" do
    it "should map good state correctly" do
      MachinePacker.convert_state(double(state: :good)).state.should eq MachineStateProto::State::GOOD
    end

    it "should map changing state correctly" do
      MachinePacker.convert_state(double(state: :changing)).state.should eq MachineStateProto::State::CHANGING
    end

    it "should map sw_failing state correctly" do
      MachinePacker.convert_state(double(state: :sw_failing)).state.should eq MachineStateProto::State::SW_FAILING
    end

    it "should map hw_failing state correctly" do
      MachinePacker.convert_state(double(state: :hw_failing)).state.should eq MachineStateProto::State::HW_FAILING
    end
  end

  context "packaging single cloaks" do
    it "should pack a tpm cloak into a MachineProto message for physical host" do
      cloak = double(id: 1, name: "test", tpm: true, state: :good)
      machine = MachinePacker.package_cloak cloak
      machine.machine_id.should eq cloak.id
      machine.name.should eq cloak.name
      machine.type.should eq MachineProto::MachineType::PHYSICAL
    end

    it "should pack a non-tpm cloak into a MachineProto message for VM host" do
      cloak = double(id: 1, name: "test", tpm: false, state: :good)
      machine = MachinePacker.package_cloak cloak
      machine.machine_id.should eq cloak.id
      machine.name.should eq cloak.name
      machine.type.should eq MachineProto::MachineType::VM
      machine.state.state.should eq MachineStateProto::State::GOOD
    end
  end

  context "packaging multiple cloaks" do
    it "should package a list of cloaks into a MachinesProto" do
      cloak1 = double(id: 1, name: "test", tpm: false, state: :good)
      cloak2 = double(id: 2, name: "test", tpm: false, state: :good)
      cloak3 = double(id: 3, name: "test", tpm: false, state: :good)
      cloaks = [cloak1, cloak2, cloak3]
      machines = MachinePacker.package_cloaks cloaks

      cloaks.each_index do |i|
        machines.machines[i].should eq MachinePacker.package_cloak cloaks[i]
      end
    end

    it "should create a valid protocol buffer" do
      cloak = double(id: 1, name: "test", tpm: false, state: :good)
      proto = MachinePacker.package_cloaks [cloak]
      expect { proto.encode }.to_not raise_exception
    end
  end
end
