class PendingResult < ActiveRecord::Base
  belongs_to :query
  before_validation :generate_auth_token

  def generate_auth_token
    o =  [('a'..'z'),('A'..'Z')].map{|i| i.to_a}.flatten
    token = (0...30).map{ o[rand(o.length)] }.join while PendingResult.where(auth_token: token).size != 0
    self.auth_token = token
  end
end
