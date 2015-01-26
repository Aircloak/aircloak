class RaTaskCodeRepeatedAnswer < ActiveRecord::Base
  belongs_to :ra_task_code
  belongs_to :repeated_answer
end
