class ClientFile < ActiveRecord::Base
  belongs_to :client_file_type
  has_many :client_file_versions

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
