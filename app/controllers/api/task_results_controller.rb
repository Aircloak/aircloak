class Api::TaskResultsController < ApplicationController
  filter_access_to :index, require: :anon_read
  before_action :authenticate_api_user
  before_action :load_task, only: [:index]
  respond_to :json

  def index
    page = (params[:page] || 1).to_i
    per_page = (params[:per_page] || 10).to_i

    query = Result.where(task: @task)

    if (params[:from])
      query = query.where("created_at >= ?", DateTime.parse(params[:from]))
    end

    if (params[:to])
      query = query.where("created_at <= ?", DateTime.parse(params[:to]))
    end

    respond_with(
          success: true,
          count: query.count,
          page: page,
          per_page: per_page,
          results:
              query.order("created_at desc").
                  paginate(page: page, per_page: per_page).
                  map {|result| result.to_client_hash}
        )
  end

  private
    def load_task
      @task = current_user.analyst.shared_tasks.find_by_token(params[:task_id])
      @task = current_user.analyst.private_tasks(current_user).find_by_token(params[:task_id]) unless @task
      respond_with({success: false, error: "Task not found."}, {status: :unprocessable_entity}) if !@task
    end
end
