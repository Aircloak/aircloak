require './lib/token_generator'

describe TokenGenerator do
  it "should generate alphanumerical tokens" do
    100.times do
      token = TokenGenerator.token_of_length 100
      (token =~ /[^\w_]/i).should eq nil
    end
  end

  it "should generate tokens of variable_length" do
    1.upto(100) do |length|
      TokenGenerator.token_of_length(length).size.should eq length
    end
  end
end
