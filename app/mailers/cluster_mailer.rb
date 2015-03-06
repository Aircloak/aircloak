class ClusterMailer < ActionMailer::Base
  default from: "no-reply@aircloak.com"

  def status_mail cluster, status_proto
    @cluster = cluster
    @description = status_proto.description
    @alterations = Alteration.where(
        "created_at >= :last_active",
        target_type: "Cluster",
        target_id: @target_id,
        last_active: @cluster.last_active
      ).order(created_at: :desc)
    mail to: "everyone-dev@aircloak.com", subject: "#{cluster.status}: #{cluster.name}"
  end
end
