require './lib/proto/air/aggregate_results.pb'

class Api::TasksController < ApplicationController
  filter_access_to [:index], require: :anon_read
  skip_before_action :verify_authenticity_token
  before_action :authenticate_api_analyst
  respond_to :json

  def set_layout
    self.class.layout false
  end

  # GET /api/tasks
  def index
    tasks = @analyst.tasks.order(:created_at).map do |task|
      {token: task.token, name: task.name, cluster_name: task.cluster.name, type: task.type_string}
    end

    respond_with({success: true, tasks: tasks}, status: :ok)
  end
end
