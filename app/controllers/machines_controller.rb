require './lib/proto/air/management_messages.pb'
require './lib/machine_packer'

class MachinesController < ApplicationController
  filter_access_to :index, require: :anon_read
  filter_access_to :update, require: :anon_read

  def index
    render text: MachinePacker.package_cloaks(Cloak.all).encode.buf, layout: false
  end

  def update
    ms = MachineStateProto.decode(request.body.read)
    cloak = Cloak.find(params[:id])
    cloak.set_health(state_to_health ms.state)
    if cloak.save
      render text: "Yeah, new state!  Finally something happening!", layout: false
    else
      render text: "I cannot do that Dave.", status: 400, layout: false
    end
  end

private
  def state_to_health state
    return :good if state == MachineStateProto::State::GOOD
    return :changing if state == MachineStateProto::State::CHANGING
    return :sw_failing if state == MachineStateProto::State::SW_FAILING
    return :hw_failing if state == MachineStateProto::State::HW_FAILING
  end
end
