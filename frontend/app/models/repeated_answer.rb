class RepeatedAnswer < ActiveRecord::Base
  belongs_to :analyst
  belongs_to :cluster

  validates_presence_of :bucket_label, :bucket_count, :timestamp, :source_ip, :noise_sd

  def timestamp_in_UTC
    Time.at(timestamp).utc.strftime('%Y-%m-%d %H:%M')
  end

  def cluster_name
    if cluster.nil?
      "<removed cluster>"
    else
      cluster.name
    end
  end

  def bucket_label_and_value
    if bucket_value
      "#{bucket_label}: #{bucket_value}"
    else
      bucket_label
    end
  end

  def all_codes_trustworthy?
    return cluster.ra_task_codes.all? { |task_code| task_code.trustworthy }
  end

  def mark_resolved
    self.resolved = true
    raise "cannot save" unless self.save
    cluster.ra_task_codes.each { |task_code| task_code.conditionally_mark_answers_resolved }
  end

  def mark_resolved_if_trustworthy
    mark_resolved if all_codes_trustworthy?
  end

  def auto_resolve?
    if not self.resolved and all_codes_trustworthy?
      self.resolved = true
      raise "cannot save" unless self.save
    end
    return self.resolved
  end

  def self.from_json cloak_ip, raw_json
    cluster = Cloak.find_by_ip(cloak_ip).cluster
    # First save the task codes (even if we cannot save the repeated answer, we want the task codes to be
    # stored in the web).
    raw_json["task_codes"].each do |task_json|
      code = RaTaskCode.from_json task_json
      cluster.add_task_code code
    end
    # Now create the new repeated answer report.
    answer = RepeatedAnswer.new(
      resolved: false,
      analyst_id: Analyst.find(raw_json["analyst"]).id,
      cluster_id: cluster.id,
      bucket_label: raw_json["bucket_label"],
      bucket_value: raw_json["bucket_value"],
      bucket_count: raw_json["bucket_count"],
      timestamp: raw_json["timestamp"],
      source_ip: raw_json["source_ip"],
      noise_sd: raw_json["noise_sd"]
    )
    raise "cannot save" unless answer.save
    return answer
  end

  # Removes all repeated answer for which there is no longer
  # an analyst. These are dangling reports from earlier times
  # before we destroyed reports along with the analysts.
  def self.remove_orphaned_reports
    sql = "DELETE FROM repeated_answers WHERE analyst_id not in (SELECT id from analysts)"
    ActiveRecord::Base.connection.execute(sql)
  end
end
