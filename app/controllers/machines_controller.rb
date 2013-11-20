require './lib/proto/air/management_messages.pb'
require './lib/machine_packer'

class MachinesController < ApplicationController
  filter_access_to :index, require: :anon_read
  filter_access_to :broken, require: :anon_write
  filter_access_to :synchronize, require: :anon_write

  def index
    render text: MachinePacker.package_cloaks(Cloak.all).encode.buf, layout: false
  end

  def broken
    cloak_where = Cloak.where(id: params[:id])
    if cloak_where.size > 0
      cloak = cloak_where.first
      cloak.good = false
      if cloak.save
        render text: "*Sniff*, another one left the world of the living!", layout: false
      else
        render text: "I cannot do that Dave!", status: 400, layout: false
      end
    else
      render text: "Do you even know what machines we have!?", status: 404, layout: false
    end
  end

  def synchronize
    cloak_where = Cloak.where(id: params[:id])
    if cloak_where.size > 0
      cloak = cloak_where.first
      if cloak.cluster_cloak
        if cloak.cluster_cloak.synchronize
          render text: "Synchronization done!", layout: false
        else
          render text: "Synchronization impossible!", status: 400, layout: false
        end
      else
        render text: "The machine does not belong to a cluster!", status: 400, layout: false
      end
    else
      render text: "We do not have this machine!", status: 404, layout: false
    end
  end
end
