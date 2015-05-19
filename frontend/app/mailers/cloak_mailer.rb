class CloakMailer < AircloakMailer
  def broken_cloak cloak
    @cloak = cloak
    mail to: "everyone-dev@aircloak.com", subject: "Broken: #{cloak.name}"
  end
end
