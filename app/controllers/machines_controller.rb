require './lib/proto/air/management_messages.pb'
require './lib/machine_packer'

class MachinesController < ApplicationController
  filter_access_to :index, require: :anon_read
  filter_access_to :broken, require: :anon_write

  def index
    render text: MachinePacker.package_cloaks(Cloak.all).encode.buf, layout: false
  end

  def broken
    cloak = Cloak.find(params[:id])
    cloak.good = false
    if cloak.save
      render text: "*Sniff*, another one left the world of the living!", layout: false
    else
      render text: "I cannot do that Dave!", layout: false
    end
  end
end
