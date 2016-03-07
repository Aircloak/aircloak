class DeployableEntityVersionsController < ApplicationController
  def index
    @deployable_entity = DeployableEntity.find(params[:deployable_entity_id])
  end

  def show
    @deployable_entity_version = DeployableEntityVersion.find(params[:id])
  end

  def reset
    @deployable_entity_version = DeployableEntityVersion.find(params[:id])
    @deployable_entity_version.reset_build_status
    render :show
  end
end
