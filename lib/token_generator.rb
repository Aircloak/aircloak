class TokenGenerator
  def self.token_of_length length
    o =  [('a'..'z'),('A'..'Z'),('0'..'9')].map{|i| i.to_a}.flatten
    token = (0...length).map{ o[rand(o.length)] }.join 
  end
end
