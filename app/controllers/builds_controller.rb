require './lib/build_versions_assigner.rb'

class BuildsController < ApplicationController
  before_action :set_build, only: [:show, :edit, :update, :destroy]

  def index
    @builds = Build.all
  end

  def new
    @build = Build.new
  end

  def edit
  end

  def create
    @build = Build.new(build_params)
    if @build.save
      BuildVersionsAssigner.assign_from_params @build, params
      redirect_to builds_path, notice: "Build with name #{@build.name} was" +
          " successfully created."
    else
      render action: 'new'
    end
  end

  def update
    if @build.update(build_params)
      BuildVersionsAssigner.assign_from_params @build, params
      redirect_to builds_path, notice: "Build with name #{@build.name} was " +
          "successfully updated."
    else
      render action: 'edit'
    end
  end

  def destroy
    name = @build.name
    @build.destroy
    redirect_to builds_path, notice: "Build with name #{name} " +
        "was successfully destroyed."
  end

private
  def set_build
    @build = Build.find(params[:id])
  end

  def build_params
    params.require(:build).permit(:name, :tpm)
  end
end
