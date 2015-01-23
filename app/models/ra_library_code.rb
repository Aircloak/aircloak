class RaLibraryCode < ActiveRecord::Base
  has_many :ra_library_code_ra_task_codes

  has_many :ra_task_codes, through: :ra_library_code_ra_task_codes

  validates_presence_of :code
  validates_uniqueness_of :code
end
