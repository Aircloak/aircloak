class TestMailer < AircloakMailer
  def test_failed test_result
    @test_result = test_result
    mail to: "everyone-dev@aircloak.com", subject: "Failure: Nightly cloak regression test"
  end
end
