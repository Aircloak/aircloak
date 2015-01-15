class CapabilitiesController < ApplicationController
  before_action :load_capabilities

  def index
    @capability = Capability.new
  end

  def create
    @capability = Capability.new capability_params
    if @capability.save
      redirect_to capabilities_path, notice: "Added capability"
    else
      render :index, error: "Couldn't save capability"
    end
  end

  def destroy
    Capability.destroy params[:id]
    redirect_to capabilities_path, notice: "Removed capability"
  end

private
  def load_capabilities
    @capabilities = Capability.all
  end

  def capability_params
    params.require(:capability).permit(:name, :identifier, :description)
  end
end
