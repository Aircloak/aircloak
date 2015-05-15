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
end
