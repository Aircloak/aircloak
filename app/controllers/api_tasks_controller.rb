require './lib/proto/air/aggregate_results.pb'

class ApiTasksController < ApplicationController
  filter_access_to [:execute_as_batch_task], require: :anon_write
  skip_before_action :verify_authenticity_token
  before_filter :assign_task, only: [:execute_as_batch_task]

  def set_layout
    self.class.layout false
  end

  # POST /api/tasks/:id/execute_as_batch_task
  def execute_as_batch_task
    @task.execute_batch_task
    render text: "Either it succeeds or not, nobody knows!"
  end

private
  def assign_task
    id = params[:id]
    @task = id.to_i == 0 ? Task.find_by_name(id) : Task.find(id)
  rescue ActiveRecord::RecordNotFound
  ensure
    render text: "Unknown task!", status: 404 unless @task
  end
end
