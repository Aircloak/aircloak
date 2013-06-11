class QueriesController < ApplicationController
  filter_access_to :execute_as_batch_query, require: :manage
  before_action :set_query, only: [:edit, :update, :destroy, :execute_as_batch_query]

  # GET /queries
  def index
    @queries = Query.all
  end

  # GET /queries/new
  def new
    @query = Query.new()
  end

  # GET /queries/1/edit
  def edit
  end

  # POST /queries
  def create
    @query = Query.new(query_params)
    @query.query_files = construct_query_files
    if @query.save
      redirect_to queries_path, notice: 'Query was successfully created.'
    else
      render action: 'new'
    end
  end

  # PATCH/PUT /queries/1
  def update
    @query.query_files = construct_query_files
    if @query.update(query_params)
      redirect_to queries_path, notice: 'Query was successfully updated.'
    else
      render action: 'edit'
    end
  end

  # DELETE /queries/1
  def destroy
    @query.destroy
    redirect_to queries_path
  end

  def execute_as_batch_query
    @query.execute_batch_query
    redirect_to queries_path
  end

private
  def construct_query_files
    query_files = ActiveSupport::JSON.decode(params[:query_files])
    files_to_return = []
    query_files.each do |qf|
      q = QueryFile.perform_json_ops qf
      next unless q
      q.add_indices_from_json qf
      q.save
      files_to_return << q
    end
    files_to_return
  end

  # Use callbacks to share common setup or constraints between actions.
  def set_query
    @query = Query.find(params[:id])
  end

  # Never trust parameters from the scary internet, only allow the white list through.
  def query_params
    params.require(:query).permit(:name, :index_id, :update_query, :identifier, :system_query, :mutator)
  end
end
