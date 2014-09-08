class RepeatedAnswer < ActiveRecord::Base
  belongs_to :analyst
  validates_presence_of :bucket_label, :bucket_count, :timestamp, :source_ip, :anonparam_k1, :anonparam_k2,
      :anonparam_target_error, :anonparam_sigma, :anonparam_constant_noise_sd
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

  def anonymization_parameters
    "k1=#{anonparam_k1}, k2=#{anonparam_k2}, target_error=#{anonparam_target_error}, " +
        "sigma=#{anonparam_sigma}, constant_noise_sd=#{anonparam_constant_noise_sd}"
  end
end
