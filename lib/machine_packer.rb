require './lib/proto/air/management_messages.pb'

class MachinePacker
  def self.package_cloak cloak
    MachinePB.new(
      id: cloak.id,
      name: cloak.name,
      type: cloak.tpm ? MachinePB::MachineType::PHYSICAL : MachinePB::MachineType::VM,
      good: cloak.good
    )
  end

  def self.package_cloaks cloaks
    MachinesPB.new(machines: cloaks.map {|m| package_cloak m})
  end
end
