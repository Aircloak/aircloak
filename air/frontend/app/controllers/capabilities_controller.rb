class CapabilitiesController < ApplicationController
  before_action :load_capabilities

  def index
    @capability = Capability.new
  end

private
  def load_capabilities
    @capabilities = Capability.all
  end

  def capability_params
    params.require(:capability).permit(:name, :identifier, :description)
  end
end
