class RepeatedAnswerMailer < ActionMailer::Base
  default from: "no-reply@aircloak.com"

  def new_report report
    @report = report
    @trustworthies = @report.ra_task_codes.where(trustworthy: true)
    @not_trustworthies = @report.ra_task_codes.where(trustworthy: false)
    @task_codes = @trustworthies + @not_trustworthies
    mail to: "everyone-dev@aircloak.com",
        subject: "Repeated answers detected for analyst: #{report.analyst.name}"
  end
end
