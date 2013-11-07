class ClustersController < ApplicationController
  before_action :set_cluster, only: [:destroy, :show, :edit]

  def index
    @builds = Build.all
    @clusters = Cluster.all
    @cluster = Cluster.new
  end

  def new
    @cluster = Cluster.new
  end

  def create
    @cluster = Cluster.create(cluster_params, tpm: Build.find(params[:build_id]).tpm)

    if @cluster.save
      redirect_to cluster_path, notice: 'Cluster was successfully created.'
    else
      render action: 'new'
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
    params.require(:name, :build_id)
  end
end
