# This file is copied to spec/ when you run 'rails generate rspec:install'
ENV["RAILS_ENV"] ||= 'test'
require File.expand_path("../../config/environment", __FILE__)
require 'rspec/rails'
require 'rspec/autorun'
require './spec/slim_helpers.rb'

require 'declarative_authorization/maintenance'
include Authorization::TestHelper

require "authlogic/test_case"
include Authlogic::TestCase

# Requires supporting ruby files with custom matchers and macros, etc,
# in spec/support/ and its subdirectories.
Dir[Rails.root.join("spec/support/**/*.rb")].each { |f| require f }

# Checks for pending migrations before tests are run.
# If you are not using ActiveRecord, you can remove this line.
ActiveRecord::Migration.check_pending! if defined?(ActiveRecord::Migration)

RSpec.configure do |config|
  # Run specs in random order to surface order dependencies. If you find an
  # order dependency and want to debug it, you can fix the order by providing
  # the seed, which is printed after each run.
  #     --seed 1234
  # For example:
  # config.seed = 1234
  config.order = "random"
end

def log_in(user, password)
  user.should_not be_nil
  session = UserSession.create! login: user.login, password: password
  session.should be_valid
  session.save
  cookies['user_credentials'] = "#{user.persistence_token}::#{user.send(user.class.primary_key)}"
end
