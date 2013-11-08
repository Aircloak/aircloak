class Cloak < ActiveRecord::Base
  belongs_to :cluster
  validates :ip, format: { with: /\A(((25[0-5])|(2[0-4][0-9])|([01]?[0-9][0-9]?))\.){3}((25[0-5])|(2[0-4][0-9])|([01]?[0-9][0-9]?))/}
  validates_presence_of :name
  validates_uniqueness_of :name, :ip

  def health
    case raw_health
    when 0 then :ok
    when 1 then :changing
    when 2 then :down
    else :unknown
    end
  end
end
