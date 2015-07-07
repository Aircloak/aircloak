require './lib/token_generator'

class AnalystToken < ActiveRecord::Base
  belongs_to :user
  # TODO (#673): remove the reference to analyst and rename the model to UserToken
  belongs_to :analyst

  TOKEN_PURPOSES = {
    api: 1
  }

  def self.create_api_token(user)
    entry = create_new(user, TOKEN_PURPOSES[:api])
    entry
  end

  def self.api_user(token)
    self.user(token, TOKEN_PURPOSES[:api])
  end

  def self.create_new(user, purpose)
    begin
      token = TokenGenerator.generate_random_string_of_at_least_length 30
    end while AnalystToken.where(token: token).count != 0
    self.create(user: user, analyst: user.analyst, purpose: purpose, token: token)
  end

  def self.user(token, purpose)
    entry = self.find_by_token(token)
    if entry.nil? || entry.purpose != purpose
      nil
    else
      entry.user
    end
  end
end