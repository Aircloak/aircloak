require './lib/proto/air/management_messages.pb'
require './lib/machine_packer'

class MachinesController < ApplicationController
  filter_access_to :index, require: :anon_read

  def index
    render text: MachinePacker.package_cloaks(Cloak.all).encode.buf, layout: false
  end

  def update
    ms = MachineStateProto.decode(request.body.read)
    cloak = Cloak.find(params[:id])
    cloak.raw_health = case ms.state
    when :GOOD then 0
    when :CHANGING then 1
    when :SW_FAILING then 2
    when :HW_FAILING then 3
    else 10000 # big number which is not allowed...
    end
    if cloak.save
      render text: "Yeah, new state!  Finally something happening!", layout: false
    else
      render text: "I cannot do that Dave.", status: 400, layout: false
    end
  end
end
