class ClustersController < ApplicationController
  before_action :set_cluster, only: [:show, :edit, :update, :destroy, :confirm_destroy]
  filter_access_to [:confirm_destroy], require: :manage

  def index
    @clusters = Cluster.all
    @cluster = Cluster.new
  end

  def new
    @cluster = Cluster.new
  end

  def create
    @cluster = Cluster.new(cluster_params)
    @cluster.log_alteration "Created with name '#{@cluster.name}' and build '#{@cluster.build.name}'."
    update_cluster 'Cluster was successfully created.', 'new'
  end

  def update
    update_cluster 'Cluster was successfully updated.', 'edit'
  end

  def destroy
    if params["cluster_name"] != @cluster.name then
      flash[:error] = "Entered name does not match cluster name!"
      redirect_to confirm_destroy_cluster_path
      return
    end
    @cluster.initiate_destroy!
    describe_successful_activity "Successfully marked cluster to be destroyed."
    redirect_to clusters_path, notice: 'Cluster was marked to be destroyed.'
  end

  def confirm_destroy
  end

  def show
  end

  def edit
  end

private
  def update_cluster msg, action
    cloaks = cloaks_from_params
    analysts = analysts_from_params
    if @cluster.assign_analysts(analysts) && @cluster.assign_cloaks(cloaks) && @cluster.update_params(cluster_params) then
      @cluster.mark_as_changed
      @cluster.save!
      redirect_to clusters_path, notice: msg
    else
      render action: action
    end
  rescue Exception => error
    flash[:error] = error
    render action: action
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

  def analysts_from_params
    return [] unless params["analyst_selections"]
    analyst_ids = params["analyst_selections"].map(&:to_i)
    analyst_ids.map {|id| Analyst.find(id)}
  end
end
