class RepeatedAnswerLibraryCode < ActiveRecord::Base
  belongs_to :repeated_answer_task_code
  validates_presence_of :name, :code
end
