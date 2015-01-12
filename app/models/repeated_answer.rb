class RepeatedAnswer < ActiveRecord::Base
  belongs_to :analyst
  validates_presence_of :bucket_label, :bucket_count, :timestamp, :source_ip, :noise_sd
  has_many :repeated_answer_task_codes

  def timestamp_in_UTC
    Time.at(timestamp).utc.strftime('%Y-%m-%d %H:%M')
  end

  def bucket_label_and_value
    if bucket_value
      "#{bucket_label}: #{bucket_value}"
    else
      bucket_label
    end
  end

  # Removes all repeated answer for which there is no longer
  # an analyst. These are dangling reports from earlier times
  # before we destroyed reports along with the analysts.
  def self.remove_orphaned_reports
    sql = "DELETE FROM repeated_answers WHERE analyst_id not in (SELECT id from analysts)"
    ActiveRecord::Base.connection.execute(sql)
  end
end
