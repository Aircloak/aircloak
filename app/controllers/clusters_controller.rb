require './lib/cluster_cloaks_assigner.rb'

class ClustersController < ApplicationController
  before_action :set_cluster, only: [:destroy, :show, :edit, :update]

  def index
    @clusters = Cluster.all
    @cluster = Cluster.new
  end

  def new
    @cluster = Cluster.new
  end

  def create
    @cluster = Cluster.create(cluster_params)
    @cluster.tpm = Build.find(params[:cluster]["build_id"].to_i).tpm
    cloaks = ClusterCloaksAssigner.get_cloaks params
    if @cluster.check_cloaks cloaks
      ClusterCloaksAssigner.assign_from_list @cluster, cloaks

      if @cluster.save
        redirect_to clusters_path, notice: 'Cluster was successfully created.'
      else
        render action: 'new'
      end
    else
      @cluster.errors.add :cloaks, "tpm must match for cloaks and build"
      render action: 'new'
    end
  end

  def update
    cloaks = ClusterCloaksAssigner.get_cloaks params
    if @cluster.check_cloaks cloaks
      ClusterCloaksAssigner.assign_from_list @cluster, cloaks
      if @cluster.update(cluster_params)
        redirect_to clusters_path, notice: 'Cluster was successfully updated.'
      else
        render action: 'edit'
      end
    else
      @cluster.errors.add :cloaks, "tpm must match for cloaks and build"
      render action: 'edit'
    end
  end

  def destroy
    @cluster.destroy
  end

  def show
  end

  def edit
  end

private
  def set_cluster
    @cluster = Cluster.find(params[:id])
  end

  def cluster_params
    params.require(:cluster).permit(:name, :build_id)
  end
end
