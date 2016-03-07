# test_item_vms are instances of virtual machines defined by a test_vm that are created for a specific test
# defined by a test_item.
class TestItemVm < ActiveRecord::Base
  validates_presence_of :name, :cpus, :memory_size, :memory_usage, :disk_usage
  belongs_to :test_item
  belongs_to :test_vm
end
