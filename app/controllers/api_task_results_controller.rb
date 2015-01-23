class ApiTaskResultsController < ApplicationController
  filter_access_to :index, require: :anon_read
  before_action :authenticate_api_analyst
  before_action :load_task, only: [:index]
  respond_to :json

  def index
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
                  map {|result| result.to_client_hash}
        )
  end

  private
    def load_task
      @task = @analyst.tasks.find_by_token params[:api_task_id]
      respond_with({success: false, error: "Task not found."}, {status: :unprocessable_entity}) if @task.nil?
    end
end
