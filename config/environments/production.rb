require 'syslog/logger'

Web::Application.configure do
  # Settings specified here will take precedence over those in config/application.rb.

  # Code is not reloaded between requests.
  config.cache_classes = true

  # Eager load code on boot. This eager loads most of Rails and
  # your application in memory, allowing both thread web servers
  # and those relying on copy on write to perform better.
  # Rake tasks automatically ignore this option for performance.
  config.eager_load = true

  # Full error reports are disabled and caching is turned on.
  config.consider_all_requests_local       = false
  config.action_controller.perform_caching = true

  # Enable Rack::Cache to put a simple HTTP cache in front of your application
  # Add `rack-cache` to your Gemfile before enabling this.
  # For large-scale production use, consider using a caching reverse proxy like nginx, varnish or squid.
  # config.action_dispatch.rack_cache = true

  # Disable Rails's static asset server (Apache or nginx will already do this).
  config.serve_static_assets = false

  # Compress JavaScripts and CSS.
  config.assets.js_compressor  = :uglifier
  # config.assets.css_compressor = :sass

  # Whether to fallback to assets pipeline if a precompiled asset is missed.
  config.assets.compile = true
  config.assets.enabled = true

  # Generate digests for assets URLs.
  config.assets.digest = true

  # Version of your assets, change this if you want to expire all your assets.
  config.assets.version = '1.0'

  # Specifies the header that your server uses for sending files.
  # config.action_dispatch.x_sendfile_header = "X-Sendfile" # for apache
  # config.action_dispatch.x_sendfile_header = 'X-Accel-Redirect' # for nginx

  # Force all access to the app over SSL, use Strict-Transport-Security, and use secure cookies.
  # config.force_ssl = true

  # Set to :debug to see everything in the log.
  config.log_level = :info

  # Prepend all log lines with the following tags.
  config.log_tags = [ :subdomain, :uuid ]

  # Use a different logger for distributed setups.
  config.logger = ActiveSupport::TaggedLogging.new(Syslog::Logger.new "web")

  # Use a different cache store in production.
  # config.cache_store = :mem_cache_store

  # Enable serving of images, stylesheets, and JavaScripts from an asset server.
  # config.action_controller.asset_host = "http://assets.example.com"

  # Additional assets to be precompiled are added
  # in config/initializers/assets.rb

  # Mailer config
  config.action_mailer.raise_delivery_errors = true
  config.action_mailer.default_options from: "no-reply@aircloak.com"
  config.action_mailer.delivery_method = :smtp

  # We cannot use the "choices" configuration gem for reading the
  # values needed by the email settings, as the configuration is loaded
  # earlier than the configuration library.
  # In this case we should be fine as the Mandrill password has been
  # restricted to only be valid when used from the following host:
  # - 139.19.208.225
  # When adding new hosts, please add the IP of the host to
  # the key in the mandrillapp.com interface.
  config.action_mailer.smtp_settings = {
    address: "smtp.mandrillapp.com",
    port: 587,
    user_name: "sebastian@aircloak.com",
    password: "jScKj_mqhGXaI_SPhUMBeg",
    domain: "aircloak.com",
    authentication: :plain
  }

  # Enable locale fallbacks for I18n (makes lookups for any locale fall back to
  # the I18n.default_locale when a translation can not be found).
  config.i18n.fallbacks = true

  # Send deprecation notices to registered listeners.
  config.active_support.deprecation = :notify

  # Disable automatic flushing of the log to improve performance.
  # config.autoflush_log = false

  # Use default logging formatter so that PID and timestamp are not suppressed.
  config.log_formatter = ::Logger::Formatter.new
end
