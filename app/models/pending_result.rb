require './lib/token_generator'

class PendingResult < ActiveRecord::Base
  belongs_to :task
  before_validation :generate_auth_token

  def generate_auth_token
    return unless self.auth_token.nil?
    begin
      token = TokenGenerator.generate_random_string_of_at_least_length 30
    end while PendingResult.where(auth_token: token).count != 0
    self.auth_token = token
  end

  def self.delete_for_task task, begin_date = nil, end_date = nil
    if begin_date.nil? or end_date.nil? then
      PendingResult.where(task_id: task.id).delete_all
    else
      PendingResult.where(task_id: task.id).where(:created_at => begin_date..end_date).delete_all
    end
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

    response = JsonSender.request(:get, :task_runner, task.analyst, task.cluster,
        URI.encode("task/#{progress_handle}"), {}, nil)
    if response["success"] == true then
      {
        label: "started at #{Time.at(self.created_at).utc.strftime('%Y-%m-%d %H:%M')}",
        progress: response["progress"]
      }
    else
      nil
    end
  end

private
  def channel_name
    "query_result_#{auth_token.gsub("-", "")}"
  end
end
