require './lib/machine_packer'

class MachinesController < ApplicationController
  filter_access_to :index, require: :anon_read

  def index
    render text: MachinePacker.package_cloaks(Cloak.all).encode.buf, layout: false
  end
end
