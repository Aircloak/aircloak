class OsTag < ActiveRecord::Base
  has_many :clusters

  validates_presence_of :name, :description
  before_destroy :can_destroy_validation

  def can_destroy?
    return false if clusters.count > 0
    true
  end

private
  def can_destroy_validation
    if not can_destroy?
      self.errors.add(:os_tag, "Cannot destroy an os tag that is in use")
      false
    end
  end
end
