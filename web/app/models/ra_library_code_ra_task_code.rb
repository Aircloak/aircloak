class RaLibraryCodeRaTaskCode < ActiveRecord::Base
  belongs_to :ra_library_code
  belongs_to :ra_task_code

  validates_presence_of :name
end
