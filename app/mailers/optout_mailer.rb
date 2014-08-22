class OptoutMailer < ActionMailer::Base
  default from: "no-reply@aircloak.com"

  def opted_out user
    @user = user
    mail to: "solutions@aircloak.com", subject: "User opted out from metrics: #{user.login}"
  end
end
