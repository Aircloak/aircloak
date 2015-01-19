class TaskResultsController < ApplicationController
  filter_access_to :show, require: :anon_read
  before_action :authenticate_analyst
  before_action :load_task, only: [:show]
  respond_to :json

  def show
    page = (params[:page] || 1).to_i
    per_page = (params[:per_page] || 10).to_i
    respond_with(
          success: true,
          count: @task.results.count,
          page: page,
          per_page: per_page,
          items:
              @task.results.order("created_at desc").
                  paginate(page: page, per_page: per_page).
                  map {|result| result.to_client}
        )
  end

  private
    def authenticate_analyst
      if request.headers["analyst"].nil?
        respond_with({success: false, error: "Missing authentication key."}, {status: :unauthorized})
      else
        @analyst = Analyst.find_by_id(request.headers["analyst"])
        respond_with({success: false, error: "Not authenticated."}, {status: :unauthorized}) if @analyst.nil?
      end
    end

    def load_task
      @task = @analyst.tasks.find_by_id params[:id]
      respond_with({success: false, error: "Task not found."}, {status: :unprocessable_entity}) if @task.nil?
    end
end
