class PermissionsController < ApplicationController
  before_action :set_permission, only: [:edit, :update, :destroy]

  def index
    @permissions = Permission.all
  end

  def new
    @permission = Permission.new
  end

  def create
    @permission = Permission.new(permission_params)

    if @permission.save
      redirect_to permissions_path, notice: 'Permission was successfully created.'
    else
      render action: 'new'
    end
  end

  def update
    if @permission.update(permission_params)
      redirect_to permissions_path, notice: 'Permission was successfully updated.'
    else
      render action: 'edit'
    end
  end

  def destroy
    @permission.destroy
    redirect_to permissions_url, notice: 'Permission was successfully destroyed.'
  end

  private
    def set_permission
      @permission = Permission.find(params[:id])
    end

    def permission_params
      params.require(:permission).permit(:name, :description)
    end
end
