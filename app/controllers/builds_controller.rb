require './lib/build_versions_assigner.rb'
require './lib/gh'

class BuildsController < ApplicationController
  before_action :set_build, only: [:destroy, :reset]

  def index
    @builds = Build.all
  end

  def new
    @build = Build.new
  end

  def branch_info
    branch_infos = []
    threads = []
    DeployableEntity.all.each do |de|
      threads << Thread.new do
        branch_infos << {
          id: de.id,
          repo: de.repo,
          branches: Gh.branchinfo_for_deployable_entity(de)
        }
      end
    end
    threads.each {|thread| thread.join}
    render json: branch_infos.to_json
  end

  def show
    @build = Build.find(params[:id])
  end

  def create
    @build = Build.new(build_params)
    BuildVersionsAssigner.assign_from_params @build, params
    if @build.save
      redirect_to builds_path, notice: "Build with name #{@build.name} was" +
          " successfully created."
    else
      render action: 'new'
    end
  end

  def destroy
    name = @build.name
    @build.destroy
    redirect_to builds_path, notice: "Build with name #{name} " +
        "was successfully destroyed."
  end

  def reset
    @build.reset
    redirect_to builds_path, notice: "Build restarted for build #{@build.name}"
  end

private
  def set_build
    @build = Build.find(params[:id])
  end

  def build_params
    params.require(:build).permit(:name, :manual)
  end
end
