require File.expand_path('../boot', __FILE__)

require 'rails/all'

# Assets should be precompiled for production (so we don't need the gems loaded then)
Bundler.require(*Rails.groups(assets: %w(development test)))

module Web
  class Application < Rails::Application
    # Settings in config/environments/* take precedence over those specified here.
    # Application configuration should go into files in config/initializers
    # -- all .rb files in that directory are automatically loaded.

    # Custom directories with classes and modules you want to be autoloadable.
    # config.autoload_paths += %W(#{config.root}/extras)

    # Set Time.zone default to the specified zone and make Active Record auto-convert to this zone.
    # Run "rake -D time" for a list of tasks for finding time zone names. Default is UTC.
    # config.time_zone = 'Central Time (US & Canada)'

    # The default locale is :en and all translations from config/locales/*.rb,yml are auto loaded.
    # config.i18n.load_path += Dir[Rails.root.join('my', 'locales', '*.{rb,yml}').to_s]
    # config.i18n.default_locale = :de
    config.i18n.enforce_available_locales = true

    # Validate that all the secret tokens that we expect to be present in
    # settings.local.yml have also been set.
    # This way we prevent the booting process of rails unless all the required
    # settings are in place. Checking at the time the token is needed is not ideal
    # as the tokens are often needed as a result of an API call performed by some
    # other service, in which case the error would be swallowed and hidden in some
    # log.
    config.after_initialize do
      [
        :secret_key_base,
        :github_oauth_token,
        :private_key_password,
        :travis_token
      ].each do |required_param|
        if not Rails.configuration.respond_to? required_param or eval("Rails.configuration.#{required_param}") == nil
          raise "#{required_param} is missing from settings.local.yml"
        end
      end
    end
  end
end
