class DeployableEntityVersionsController < ApplicationController
  def index
    @deployable_entity = DeployableEntity.find(params[:deployable_entity_id])
  end

  def show
    @deployable_entity_version = DeployableEntityVersion.find(params[:id])
  end
end
