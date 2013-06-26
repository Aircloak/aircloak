require './lib/proto/cloak/query.pb'
require './lib/proto/air/query_upload.pb'

class QueriesController < ApplicationController
  filter_access_to :execute_as_batch_query, require: :manage
  filter_access_to :upload_query_data, require: :anon_read
  before_action :set_query, only: [:edit, :update, :destroy, :execute_as_batch_query]
  protect_from_forgery :except => :upload_query_data 

  # GET /queries
  def index
    queries = Query.all

    @ready_queries = queries.select { |q| q.ready_for_primetime }
    @not_ready_queries = queries.select { |q| not q.ready_for_primetime }
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
    @query.destroy
    redirect_to queries_path
  end

  def execute_as_batch_query
    @query.execute_batch_query
    redirect_to queries_path
  end

  def upload_query_data
    data = request.body.read
    data_to_save = data.dup
    qd = QueryData.decode(data)
    q = Query.where(main_package: qd.main_package).first
    q = Query.new(main_package: qd.main_package) unless q
    q.packaged_data = data_to_save
    q.save(validate: false)
    render text: "Thanks"
  end

private
  # Use callbacks to share common setup or constraints between actions.
  def set_query
    @query = Query.find(params[:id])
  end

  # Never trust parameters from the scary internet, only allow the white list through.
  def query_params
    params.require(:query).permit(:name, :index_id, :update_query, :identifier, :system_query, :mutator)
  end
end
