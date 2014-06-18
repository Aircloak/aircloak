class CloakMailer < ActionMailer::Base
  default from: "no-reply@aircloak.com"

  def broken_cloak cloak
    @cloak = cloak
    mail to: "everyone-dev@aircloak.com", subject: "Broken: #{cloak.name}"
  end
end
