class ClusterMailer < ActionMailer::Base
  default from: "no-reply@aircloak.com"

  def status_mail cluster, status_proto
    @cluster = cluster
    @description = status_proto.description
    mail to: "everyone-dev@aircloak.com", subject: "#{cluster.status}: #{cluster.name}"
  end
end
