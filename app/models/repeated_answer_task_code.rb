class RepeatedAnswerTaskCode < ActiveRecord::Base
  belongs_to :repeated_answer
  validates_presence_of :prefetch, :code
end
