require './lib/token_generator'

class PendingResult < ActiveRecord::Base
  belongs_to :query
  before_validation :generate_auth_token

  def generate_auth_token
    begin
      token = TokenGenerator.token_of_length 30
    end while PendingResult.where(auth_token: token).count != 0
    self.auth_token = token
  end
end
