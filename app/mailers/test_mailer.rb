class TestMailer < ActionMailer::Base
  default from: "no-reply@aircloak.com"

  def test_failed test
    @test = test
    mail to: "everyone-dev@aircloak.com", subject: "Failed test"
  end

  def test_build_failed test
    @test = test
    mail to: "everyone-dev@aircloak.com", subject: "Build for test failed"
  end
end
