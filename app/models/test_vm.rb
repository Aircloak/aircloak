# test_vms are descriptions of virtual machine types generated to perform tests.
class TestVm < ActiveRecord::Base
  validates_presence_of :name, :duration, :disk_size, :disk_usage
  belongs_to :test_result
  has_many :test_item_vms
  has_many :test_items, through: :test_item_vms
end
