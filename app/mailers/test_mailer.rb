class TestMailer < ActionMailer::Base
  default from: "no-reply@aircloak.com"

  def test_failed test_result
    @test_result = test_result
    mail to: "everyone-dev@aircloak.com", subject: "Failure: Nightly cloak regression test"
  end
end
