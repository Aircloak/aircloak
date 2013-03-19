source 'https://rubygems.org'

gem 'rails',     github: 'rails/rails'
gem 'arel',      github: 'rails/arel'
gem 'activerecord-deprecated_finders', github: 'rails/activerecord-deprecated_finders'

gem 'haml'
# gem 'd3-rails'

gem "javaclass"
gem 'beefcake'

# We use this gem to sign the
# update commands for the client apps
gem "sshkeyauth"

group :development do
  gem 'better_errors'
  gem 'binding_of_caller'
  gem 'meta_request'
  gem 'pry'
  gem 'pry-nav'
  gem 'haml-rails', github: 'indirect/haml-rails'
  gem 'pg'
end

group :production do
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

  # For uploading query files
  gem 'jquery-fileupload-rails'

  # See https://github.com/sstephenson/execjs#readme for more supported runtimes
  gem 'therubyracer', platforms: :ruby
  gem 'uglifier', '>= 1.0.3'
end

gem 'jquery-rails'
gem 'turbolinks'

# To use ActiveModel has_secure_password
gem 'bcrypt-ruby', '~> 3.0.0'

# Deploy with Capistrano
gem 'capistrano', '2.13.5', group: :development

gem 'unicorn'

gem 'angularjs-rails'
gem 'underscore-rails'
