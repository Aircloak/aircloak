require './lib/proto/air/management_messages.pb'

class MachinePacker
  def self.convert_state cloak
    state = nil
    state = MachineStateProto::State::GOOD if cloak.state == :good
    state = MachineStateProto::State::CHANGING if cloak.state == :changing
    state = MachineStateProto::State::SW_FAILING if cloak.state == :sw_failing
    state = MachineStateProto::State::HW_FAILING if cloak.state == :hw_failing
    MachineStateProto.new state: state
  end

  def self.package_cloak cloak
    MachineProto.new(
      machine_id: cloak.id,
      name: cloak.name,
      type: cloak.tpm ? MachineProto::MachineType::PHYSICAL : MachineProto::MachineType::VM,
      state: convert_state(cloak)
    )
  end

  def self.package_cloaks cloaks
    MachinesProto.new(machines: cloaks.map {|m| package_cloak m})
  end
end
