require './lib/proto/air/management_messages.pb'
require './lib/machine_packer'

class MachinesController < ApplicationController
  filter_access_to [:index, :setup_info], require: :anon_read
  filter_access_to [:broken, :synchronize], require: :anon_write
  layout false

  def index
    render text: MachinePacker.package_cloaks(Cloak.all).encode.buf, layout: false
  end

  def broken
    cloak = Cloak.find_by_id(params[:id])
    if cloak
      if cloak.set_broken
        render text: "*Sniff*, another one left the world of the living!"
      else
        render text: "I cannot do that Dave!", status: 400
      end
    else
      render text: "Do you even know what machines we have!?", status: 404
    end
  end

  def synchronize
    cloak = Cloak.find_by_id(params[:id])
    if cloak
      if cloak.cluster_cloak
        if cloak.cluster_cloak.synchronize
          render text: "Synchronization done!"
        else
          render text: "Synchronization impossible!", status: 400
        end
      else
        render text: "The machine does not belong to a cluster!", status: 400
      end
    else
      render text: "We do not have this machine!", status: 404
    end
  end

  def setup_info
    ip = request.remote_ip
    @cloak = Cloak.find_by_ip(ip)
    unless @cloak
      logger.error "Machine with IP: #{ip} isn't a known machine. It requested setup info and will not be able to setup correctly!"
      render text: "Unknown machine!", status: 400
    end
  end
end
