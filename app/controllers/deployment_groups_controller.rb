class DeploymentGroupsController < ApplicationController
  before_action :set_deployment_group, only: [:edit, :update, :destroy]

  # GET /deployment_groups
  def index
    @deployment_groups = DeploymentGroup.all
  end

  # GET /deployment_groups/new
  def new
    @deployment_group = DeploymentGroup.new
  end

  # POST /deployment_groups
  def create
    @deployment_group = DeploymentGroup.new(deployment_group_params)

    if @deployment_group.save
      redirect_to deployment_groups_path, notice: 'Deployment group was successfully created.'
    else
      render action: 'new'
    end
  end

  # PATCH/PUT /deployment_groups/1
  def update
    if @deployment_group.update(deployment_group_params)
      redirect_to deployment_groups_path, notice: 'Deployment group was successfully updated.'
    else
      render action: 'edit'
    end
  end

  def create_command
    group = DeploymentGroup.find(params[:id])
    group.create_new_command
    redirect_to client_binaries_path
  end

  # DELETE /deployment_groups/1
  def destroy
    @deployment_group.destroy
    redirect_to deployment_groups_url, notice: 'Deployment group was successfully destroyed.'
  end

  private
    # Use callbacks to share common setup or constraints between actions.
    def set_deployment_group
      @deployment_group = DeploymentGroup.find(params[:id])
    end

    # Never trust parameters from the scary internet, only allow the white list through.
    def deployment_group_params
      params.require(:deployment_group).permit(:identifier, :name, :verified_only, :autoupdate)
    end
end
