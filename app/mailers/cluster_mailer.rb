class ClusterMailer < ActionMailer::Base
  default from: "no-reply@aircloak.com"

  def status_mail cluster, status_proto
    @cluster = cluster
    @description = status_proto.description
    @alterations = Alteration.where(
        created_at: cluster.last_active..Time.now,
        target_type: "Cluster",
        target_id: cluster.id
      ).order(created_at: :desc)
    mail to: "everyone-dev@aircloak.com", subject: "#{cluster.status.to_s.humanize}: #{cluster.name}"
  end
end
