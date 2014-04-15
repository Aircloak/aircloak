require './lib/proto/cloak/query.pb'
require './lib/proto/air/query_upload.pb'

class QueriesController < ApplicationController
  filter_access_to :execute_as_batch_query, require: :manage
  before_action :set_query, only: [:edit, :update, :destroy, :execute_as_batch_query]
  protect_from_forgery

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
    if @query.save
      redirect_to queries_path, notice: 'Query was successfully created.'
    else
      render action: 'new'
    end
  end

  # PATCH/PUT /queries/1
  def update
    if @query.update(query_params)
      redirect_to queries_path, notice: 'Query was successfully updated.'
    else
      render action: 'edit'
    end
  end

  # DELETE /queries/1
  def destroy
    @query.efficient_delete
    redirect_to queries_path
  end

  def execute_as_batch_query
    @query.execute_batch_query
    redirect_to queries_path
  end

private
  # Use callbacks to share common setup or constraints between actions.
  def set_query
    @query = Query.find(params[:id])
  end

  # Never trust parameters from the scary internet, only allow the white list through.
  def query_params
    params.require(:query).permit(:name, :task_id, :cluster_id)
  end
end
