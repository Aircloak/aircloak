class Api::TaskResultsController < ApplicationController
  filter_access_to :index, require: :anon_read
  before_action :authenticate_api_user
  before_action :load_task, only: [:index]
  respond_to :json

  def index
    page = (params[:page] || 1).to_i
    per_page = (params[:per_page] || 10).to_i
    begin_date_str = params[:from] || "19700101 00:01"
    end_date_str = params[:to] || Time.now.strftime("%Y%m%d %H:%M")
    formatted_begin_date_str = DateTime.parse(begin_date_str).strftime("%Y/%m/%d %H:%M")
    formatted_end_date_str = DateTime.parse(end_date_str).strftime("%Y/%m/%d %H:%M")
    rpc_response "task_results_json",
        [@task.id, page, per_page, formatted_begin_date_str, formatted_end_date_str]
  end

  private
    def load_task
      @task = current_user.analyst.shared_tasks.find_by_token(params[:task_id])
      @task = current_user.analyst.private_tasks(current_user).find_by_token(params[:task_id]) unless @task
      rpc_error({success: false, error: "Task not found."}, status: :unprocessable_entity) if !@task
    end
end
