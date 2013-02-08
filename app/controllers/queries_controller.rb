class QueriesController < ApplicationController
  before_action :set_query, only: [:edit, :update, :destroy]

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
    redirect_to queries_url
  end

  def upload_query_file_for_new_query
    query_file = QueryFile.from_upload params[:query_file]
    @query = Query.new(query_files: [query_file])
    render :partial => 'query_file_fields_wrapper'
  end

  def upload_query_file_for_query
    query_file = QueryFile.from_upload params[:query_file]
    @query = Query.find(params[:id])
    @query.query_files = [query_file]
    render :partial => 'query_file_fields_wrapper'
  end

  private
    def construct_query_files
      query_files = ActiveSupport::JSON.decode(params[:query_files])
      files_to_return = []
      query_files.each do |qf|
        q = nil
        if qf["fresh"] then
          if qf["scheduleRemove"] then
            TempQueryFile.destroy(qf["temp_file"])
            next
          else
            temp_file = TempQueryFile.find(qf["temp_file"])
            q = QueryFile.new(
              name: qf["name"],
              query_interface: qf["query_interface"],
              index_ops: qf["index_ops"],
              package: qf["package"],
              index_ops: qf["index_ops"],
              data: temp_file.data)
          end
        else
          if qf["scheduleRemove"] then
            QueryFile.destroy(qf["id"])
            next
          else
            q = QueryFile.find(qf["id"])
          end
        end
        qf["indices"].each do |i|
          index = nil
          if i["fresh"]
            index = Index.new(
              name: i["name"],
              human_name: i["human_name"],
              system_index: !i["system_index"])
            index.save
          else
            index = Index.find(i["id"])
          end
          q.indices << index
        end
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
