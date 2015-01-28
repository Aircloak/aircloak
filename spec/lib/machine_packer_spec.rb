require './lib/machine_packer.rb'

describe MachinePacker do
  context "packaging single cloaks" do
    it "should pack a tpm cloak into a MachinePB message for physical host" do
      cloak = double(id: 1, internal_domain: "test", tpm: true, good: true)
      machine = MachinePacker.package_cloak cloak
      machine.id.should eq cloak.id
      machine.name.should eq cloak.internal_domain
      machine.type.should eq MachinePB::MachineType::PHYSICAL
    end

    it "should pack a non-tpm cloak into a MachinePB message for VM host" do
      cloak = double(id: 1, internal_domain: "test", tpm: false, good: true)
      machine = MachinePacker.package_cloak cloak
      machine.id.should eq cloak.id
      machine.name.should eq cloak.internal_domain
      machine.type.should eq MachinePB::MachineType::VM
      machine.good.should eq true
    end
  end

  context "packaging multiple cloaks" do
    it "should package a list of cloaks into a MachinesPB" do
      cloak1 = double(id: 1, name: "cloak1", internal_domain: "test", tpm: false, good: true)
      cloak2 = double(id: 2, name: "cloak2", internal_domain: "test", tpm: false, good: true)
      cloak3 = double(id: 3, name: "cloak3", internal_domain: "test", tpm: false, good: true)
      cloaks = [cloak1, cloak2, cloak3]
      machines = MachinePacker.package_cloaks cloaks

      cloaks.each_index do |i|
        machines.machines[i].should eq MachinePacker.package_cloak cloaks[i]
      end
    end

    it "should create a valid protocol buffer" do
      cloak = double(id: 1, internal_domain: "test", tpm: false, good: true)
      proto = MachinePacker.package_cloaks [cloak]
      expect { proto.encode }.to_not raise_exception
    end
  end
end
