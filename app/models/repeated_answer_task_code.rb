class RepeatedAnswerTaskCode < ActiveRecord::Base
  belongs_to :repeated_answer
  has_many :repeated_answer_library_codes, dependent: :destroy
  validates_presence_of :prefetch, :code
end
