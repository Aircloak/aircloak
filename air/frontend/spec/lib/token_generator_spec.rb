require './lib/token_generator'

describe TokenGenerator do
  it "should generate alphanumerical tokens" do
    100.times do
      token = TokenGenerator.generate_random_string_of_at_least_length 100
      (token =~ /[^\w_\-]/i).should eq nil
    end
  end

  it "should generate tokens of variable_length" do
    1.upto(100) do |length|
      TokenGenerator.generate_random_string_of_at_least_length(length).size.should be > length
    end
  end

  it "should successfully generate valid user token" do
    analystKeyText, analystCertText = TokenGenerator.generate_root_token("analyst", 3)
    userKeyText, userCertText = TokenGenerator.generate_leaf_token(analystKeyText, analystCertText, "user", 4)
    TokenGenerator.decrypt_key_text(userKeyText)

    #File.write("analyst.key", analystKeyText)
    #File.write("analyst.crt", analystCertText)
    #File.write("user.key", userKeyText)
    #File.write("user.crt", userCertText)

    TokenGenerator.generate_subject_string("user", 4).should eq OpenSSL::X509::Certificate.new(userCertText).subject.to_s
  end
end
