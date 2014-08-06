class KeyMaterial < ActiveRecord::Base
  belongs_to :analyst

  # A key_material contains the following properties
  # - certificate: the raw certificate
  # - key: the raw key protected with the aircloak password
  # - pkcs12: a packaged version of the unprotected key and the certificate
  #     protected by a password chosen by the analyst. Not usable by aircloak.
  #     Aircloak can regenerate a pkcs12 based on the raw certificate
  #     and key.

  # We currently only support pkcs12 for uploading data for any user.
  # pkcs12's for uploading data for an individual user, or for acting as
  # an inquirer will be added when we need them.

  # Creates a new key, cert pair signed by the analysts master key.
  # A corresponding pkcs12 protected by a password specified by the
  # analyst is also generated.
  def self.create_from_analyst analyst, password, description
    key_material = analyst.key_materials.new
    raw_key, raw_cert = TokenGenerator.generate_leaf_token analyst.key, analyst.certificate, "any_user", 0

    key = TokenGenerator.import_key raw_key
    cert = OpenSSL::X509::Certificate.new raw_cert
    pkcs12 = OpenSSL::PKCS12.create(
      password,
      "#{description}. Allows uploading data for any user to any of #{analyst.name}'s clusters",
      key,
      cert
    )

    key_material.pkcs12 = Base64.encode64 pkcs12.to_der
    key_material.key = raw_key
    key_material.certificate = raw_cert
    key_material.description = description
    key_material.save
    key_material
  end
end
