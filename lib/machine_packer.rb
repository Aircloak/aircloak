require './lib/proto/air/management_messages.pb'

class MachinePacker
  def self.package_cloak cloak
    MachineProto.new(
      machine_id: cloak.id,
      name: cloak.name,
      type: cloak.tpm ? MachineProto::MachineType::PHYSICAL : MachineProto::MachineType::VM,
      good: cloak.good
    )
  end

  def self.package_cloaks cloaks
    MachinesProto.new(machines: cloaks.map {|m| package_cloak m})
  end
end
