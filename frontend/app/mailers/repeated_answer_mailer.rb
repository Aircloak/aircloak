class RepeatedAnswerMailer < AircloakMailer
  def new_report report
    @report = report
    mail to: "everyone-dev@aircloak.com",
        subject: "Repeated answers detected for analyst #{report.analyst.name} on cluster #{report.cluster_name}"
  end
end
