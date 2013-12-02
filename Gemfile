source 'https://rubygems.org'

ruby '2.0.0'

gem 'rails', '~> 4.0.0' #, github: 'rails/rails'

gem "haml", :github => "haml/haml", :branch => "stable"
# gem 'd3-rails'

gem "javaclass"
# Ruby zip 1 has broken javaclass, so we need a version that is younger
gem 'rubyzip', '< 1.0.0'

gem 'beefcake'

# We use this gem to sign the
# update commands for the client apps
gem "sshkeyauth"

# We use this for interacting with the github api
gem "github_api"
gem 'excon'

# Configuration management
gem "choices"

group :development, :test do
  gem 'pry'
  gem 'pry-nav'
  gem 'rspec-rails', '~> 2.0'
  gem 'sqlite3'
end

group :test do
  gem "codeclimate-test-reporter", require: nil
  gem 'rake'
  gem 'vcr'
end

group :development do
  gem 'better_errors'
  gem 'binding_of_caller'
  gem 'meta_request'
  gem 'haml-rails', github: 'indirect/haml-rails'
  gem 'ruby_parser'
end

group :production do
  gem 'SyslogLogger'
  gem 'pg'
end

# Gems used only for assets and not required
# in production environments by default.
group :assets do
  gem 'sprockets-rails', github: 'rails/sprockets-rails'
  gem 'sass-rails',   github: 'rails/sass-rails'
  gem 'coffee-rails', github: 'rails/coffee-rails'
  gem 'twitter-bootstrap-rails'
  gem 'less-rails'

  # See https://github.com/sstephenson/execjs#readme for more supported runtimes
  gem 'therubyracer', platforms: :ruby
  gem 'uglifier', '>= 1.0.3'
end

gem 'jquery-rails'
# gem 'turbolinks', github: 'rails/turbolinks'

# To use ActiveModel has_secure_password
gem 'bcrypt-ruby', '~> 3.0.0'

# Deploy with Capistrano
gem 'capistrano', '2.13.5', group: :development

gem 'unicorn'

gem 'authlogic'
gem 'declarative_authorization'

gem 'angularjs-rails'
gem 'underscore-rails'
