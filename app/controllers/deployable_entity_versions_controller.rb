class DeployableEntityVersionsController < ApplicationController
  def index
    @deployable_entity = DeployableEntity.find(params[:deployable_entity_id])
  end
end
