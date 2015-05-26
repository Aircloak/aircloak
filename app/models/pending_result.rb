require './lib/token_generator'

class PendingResult < ActiveRecord::Base
  belongs_to :task
  before_validation :generate_auth_token

  def generate_auth_token
    begin
      token = TokenGenerator.generate_random_string_of_at_least_length 30
    end while PendingResult.where(auth_token: token).count != 0
    self.auth_token = token
  end

  def self.delete_for_task task
    PendingResult.where(task_id: task.id).delete_all
  end

  def signal_result result
    result_id = connection.quote result.id.to_s
    connection.execute "NOTIFY #{channel_name}, #{result_id}"
  end

  def await_result
    connection.execute "LISTEN #{channel_name}"
    result = nil
    connection.raw_connection.wait_for_notify do |channel, pid, result_id|
      result = Result.find(result_id)
    end
    result
  ensure
    connection.execute "UNLISTEN *"
  end

  def progress_status
    return nil if progress_handle.nil?

    url = URI.encode(cloak_url("/task/#{progress_handle}"))
    if url
      response = JsonSender.request(:get, :task_runner, task.analyst, task.cluster,
          URI.encode("task/#{progress_handle}"), {}, nil)
      if response["success"] == true then
        {
          time: self.created_at,
          progress: response["progress"]
        }
      else
        nil
      end
    end
  end

private
  def channel_name
    "query_result_#{auth_token.gsub("-", "")}"
  end

  def cloak_url path
    raise "No cloak in cluster" unless cloak
    prot = Rails.configuration.cloak.protocol
    port = Rails.configuration.cloak.port
    return "#{prot}://#{cloak.ip}:#{port}/#{path}"
  end

  def cloak
    cluster_cloak = ClusterCloak.where(cluster_id: task.cluster_id, raw_state: ClusterCloak.state_to_raw_state(:belongs_to)).limit(1).first
    cluster_cloak.cloak if cluster_cloak
  end
end
