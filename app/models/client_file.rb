class ClientFile < ActiveRecord::Base
  belongs_to :client_file_type
  has_many :client_file_versions

  validates :local_name, uniqueness: true
  validates :name, uniqueness: true
  validates_presence_of :local_name, :name, :extension, :client_file_type_id

  def self.as_options
    ClientFile.all.map do |ft|
      [ft.name, ft.id]
    end
  end

  def self.get_most_recent_versions
    files = []
    ClientFile.all.map do |file|
      version = file.client_file_versions.order(created_at: :desc).first
      if version
        files << version
      else
        files << {name: file.name}
      end
    end
    files
  end

  def self.get_most_recent_existing_versions
    get_most_recent_versions.select do |v|
      not v.is_a? Hash
    end
  end
end
