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
    update_cloaks 'Cluster was successfully created.', 'new'
  end

  def update
    update_cloaks 'Cluster was successfully updated.', 'edit'
  end

  def destroy
    @cluster.destroy
  end

  def show
  end

  def edit
  end

private
  def update_cloaks msg, action
    cloaks = cloaks_from_params
    @cluster.assign_cloaks cloaks
    if @cluster.update(cluster_params)
      redirect_to clusters_path, notice: msg
    else
      render action: action
    end
  end

  def set_cluster
    @cluster = Cluster.find(params[:id])
  end

  def cluster_params
    params.require(:cluster).permit(:name, :build_id)
  end

  def cloaks_from_params
    return [] unless params["cloak_selections"]
    cloak_ids = params["cloak_selections"].map(&:to_i)
    cloak_ids.map {|id| Cloak.find(id)}
  end
end
