class ClientFileType < ActiveRecord::Base
  has_many :client_files

  validates :human_name, uniqueness: true
  validates :name, uniqueness: true
  validates_presence_of :human_name, :name

  def self.as_options
    ClientFileType.all.map do |ft|
      [ft.human_name, ft.id]
    end
  end
end
