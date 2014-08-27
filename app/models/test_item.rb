class TestItem < ActiveRecord::Base
  validates_presence_of :name, :duration
  belongs_to :test_result
  has_many :test_item_vms
end
