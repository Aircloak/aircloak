class RepeatedAnswer < ActiveRecord::Base
  belongs_to :analyst
  validates_presence_of :bucket_label, :bucket_count, :timestamp, :source_ip, :anonparam_k1, :anonparam_k2,
      :anonparam_target_error, :anonparam_sigma, :anonparam_constant_noise_sd
  has_many :repeated_answer_task_codes
end
