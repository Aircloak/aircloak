require 'airpub_api'
require './lib/aircloak_config'

class AirpubController < ApplicationController
  protect_from_forgery :except => :event

  def index
    @analyst_id = current_user.analyst.id unless current_user.analyst.nil?
  end

  def subscribe
    @path = params['path']
  end

  # This action supplies the valid airpub path for the requested resource.
  # We need do this on the server so we can verify that the user has proper permissions.
  # This prevents unauthorized access, such as one analyst tapping into the feed of
  # some other analyst.
  def request_parameters
    path =
      case params['resource_type']
      when 'table_stats'
        table = current_user.analyst.user_tables.find(params['table_id'])
        "/table_stats/#{table.analyst.id}/#{table.id}"

      when 'task_results'
        task = current_user.analyst.tasks.find_by_token(params['task_token'])
        raise ActionController::RoutingError.new('Not Found') if task.nil?
        "/results/#{task.analyst.id}/#{task.token}"

      when 'path'
        raise ActionController::RoutingError.new('Not Found') unless current_user.admin?
        params['path']

      else
        raise ActionController::RoutingError.new('Not Found')
      end

    describe_successful_activity(
        "Supplied airpub parameters for resource type #{params['resource_type']} at path #{path}")

    render json: {
      server: Conf.get("/service/airpub/subscribe_endpoint"),
      request: AirpubApi.generate_subscribe_request(path)
    }
  end
end
