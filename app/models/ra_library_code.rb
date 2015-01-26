class RaLibraryCode < ActiveRecord::Base
  has_many :ra_library_code_ra_task_codes

  has_many :ra_task_codes, through: :ra_library_code_ra_task_codes

  validates_presence_of :code
  validates_uniqueness_of :code

  def belongs_to_trustworthy_code
    self.ra_task_codes.where(trustworthy: true).count > 0
  end
end
