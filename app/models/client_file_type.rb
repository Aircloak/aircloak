class ClientFileType < ActiveRecord::Base
  before_destroy :can_destroy?

  has_many :client_files, dependent: :destroy

  validates :human_name, uniqueness: true
  validates :name, uniqueness: true
  validates_presence_of :human_name, :name, :extension

  def self.as_options
    ClientFileType.all.map do |ft|
      ["#{ft.human_name} ({{local_name}}.#.#{ft.extension})", ft.id]
    end
  end

  # Check all files of this type to see if they are part of active commands
  def can_destroy?
    client_files.each do |file|
      self.errors[:base] << "File type cannot be deleted while files of the type exists."
      return false unless file.can_destroy?
    end
    true
  end
end
