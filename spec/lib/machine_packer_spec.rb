require './lib/machine_packer.rb'

describe MachinePacker do
  context "packaging single cloaks" do
    it "should pack a tpm cloak into a MachineProto message for physical host" do
      cloak = double(id: 1, name: "test", tpm: true)
      machine = MachinePacker.package_cloak cloak
      machine.machine_id.should eq cloak.id
      machine.name.should eq cloak.name
      machine.type.should eq MachineProto::MachineType::PHYSICAL
    end

    it "should pack a non-tpm cloak into a MachineProto message for VM host" do
      cloak = double(id: 1, name: "test", tpm: false)
      machine = MachinePacker.package_cloak cloak
      machine.machine_id.should eq cloak.id
      machine.name.should eq cloak.name
      machine.type.should eq MachineProto::MachineType::VM
    end
  end

  context "packaging multiple cloaks" do
    it "should package a list of cloaks into a MachinesProto" do
      cloak1 = double(id: 1, name: "test", tpm: false)
      cloak2 = double(id: 1, name: "test", tpm: false)
      cloak3 = double(id: 1, name: "test", tpm: false)
      cloaks = [cloak1, cloak2, cloak3]
      machines = MachinePacker.package_cloaks cloaks

      cloaks.each_index do |i|
        machines.machines[i].should eq MachinePacker.package_cloak cloaks[i]
      end
    end
  end
end
