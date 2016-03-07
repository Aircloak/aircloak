class DeployableEntitiesController < ApplicationController
  before_action :set_deployable_entity, only: [:show, :edit, :update, :destroy]

  def index
    @deployable_entities = DeployableEntity.all
  end

  def new
    @deployable_entity = DeployableEntity.new
  end

  def edit
  end

  def create
    @deployable_entity = DeployableEntity.new(deployable_entity_params)

    if @deployable_entity.save
      redirect_to deployable_entities_path, notice: "#{@deployable_entity.repo} entity was successfully created."
    else
      render action: 'new'
    end
  end

  def update
    if @deployable_entity.update(deployable_entity_params)
      redirect_to deployable_entities_path, notice: "#{@deployable_entity.repo} was successfully updated."
    else
      render action: 'edit'
    end
  end

  def destroy
    @deployable_entity.destroy
    redirect_to deployable_entities_url, notice: 'Deployable entity was successfully destroyed.'
  end

  private
    def set_deployable_entity
      @deployable_entity = DeployableEntity.find(params[:id])
    end

    def deployable_entity_params
      params.require(:deployable_entity).permit(:repo)
    end
end
