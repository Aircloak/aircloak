source 'https://rubygems.org'

ruby '2.0.0'

gem 'rails', '~> 4.0.3' #, github: 'rails/rails'

gem "haml", github: "haml/haml", branch: "stable"

# Ruby protobuf compiler
gem 'beefcake'

gem 'rest-client'

# We use this for interacting with the github api
gem "github_api"
gem 'excon'

# Configuration management
gem "choices"

# Use to paginate the activities on the activities page
gem 'will_paginate', '~> 3.0'

# We need redcarpet for rendering markdown for our
# help sections
gem "redcarpet"
gem "coderay", github: "rubychan/coderay"

group :development, :test do
  gem 'pry', github: "pry/pry"
  gem 'pry-nav', github: "nixme/pry-nav"
  gem 'rspec-rails', '~> 2.0'
  gem 'sqlite3'
end

group :test do
  gem 'rake'
  gem 'vcr'
end

group :development do
  gem 'better_errors'
  gem 'binding_of_caller'
  gem 'meta_request'
  gem 'haml-rails', github: 'indirect/haml-rails'
  gem 'ruby_parser'
  gem 'quiet_assets' # quiet down assets from the log
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

gem "rails-backbone", :github => "codebrew/backbone-rails", tag: "v1.1.0"
gem 'underscore-rails', '~> 1.6.0'
gem 'handlebars_assets'
gem 'hamlbars', '~> 2.1'
gem 'codemirror-rails', '~> 4.5'
