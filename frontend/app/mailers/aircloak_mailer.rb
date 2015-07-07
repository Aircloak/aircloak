require './lib/aircloak_config'

# The sole purpose of this class is to dynamically load
# the right SMTP configuration, and set sensible defaults
# This is taken from: https://coderwall.com/p/tfzxyq/rails-4-dynamic-smtp-settings
# which is the same guy that landed the fix into the rails codebase
# in the first place: https://github.com/rails/rails/pull/7397
class AircloakMailer < ActionMailer::Base
  default from: "no-reply@aircloak.com"
  after_filter :set_smtp

private
  def set_smtp
    settings = {
      address: Conf.get("/settings/mail/smtp/address"),
      port: Conf.get("/settings/mail/smtp/port"),
      user_name: Conf.get("/settings/mail/smtp/username"),
      password: Conf.get("/settings/mail/smtp/password"),
      domain: Conf.get("/settings/mail/smtp/domain"),
      authentication: :plain
    }
    message.delivery_method.settings.merge!(settings)
  end
end
