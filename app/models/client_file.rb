class ClientFile < ActiveRecord::Base
  before_destroy :can_destroy?

  belongs_to :client_file_type
  has_many :client_file_versions, dependent: :destroy

  validates :local_name, uniqueness: true
  validates :name, uniqueness: true
  validates_presence_of :local_name, :name, :client_file_type_id

  def self.as_options
    ClientFile.all.map do |ft|
      [ft.name, ft.id]
    end
  end

  def self.get_most_recent_existing_versions params
    get_most_recent_versions(params).select do |v|
      not v.is_a? Hash
    end
  end

  # Checks if the versions of this file are currently in active use.
  # If so, then we cannot destroy it
  def can_destroy?
    client_file_versions.each do |version|
      self.errors[:base] << "File #{self.local_name} cannot be deleted while versions of it are still in use"
      return false if version.has_active_command?
    end
    true
  end

private
  def self.get_most_recent_versions params={:only_verified => true}
    files = []
    ClientFile.all.map do |file|
      version = nil
      if params[:only_verified] and file.requires_verifications then
        version = file.client_file_versions.where(verified:true).order(created_at: :desc).first
      else
        version = file.client_file_versions.order(created_at: :desc).first
      end
      if version
        files << version
      else
        files << {name: file.name}
      end
    end
    files
  end

end
