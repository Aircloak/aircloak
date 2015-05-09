require './lib/token_generator'

class AnalystToken < ActiveRecord::Base
  belongs_to :analyst

  TOKEN_PURPOSES = {
    api: 1
  }

  def self.create_api_token(analyst)
    entry = create_new(analyst, TOKEN_PURPOSES[:api])
    entry
  end

  def self.api_analyst(token)
    self.analyst(token, TOKEN_PURPOSES[:api])
  end

  def self.create_new(analyst, purpose)
    begin
      token = TokenGenerator.generate_random_string_of_at_least_length 30
    end while AnalystToken.where(token: token).count != 0
    self.create(analyst: analyst, purpose: purpose, token: token)
  end

  def self.analyst(token, purpose)
    entry = self.find_by_token(token)
    if entry.nil? || entry.purpose != purpose
      nil
    else
      entry.analyst
    end
  end
end