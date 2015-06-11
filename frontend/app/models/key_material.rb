class KeyMaterial < ActiveRecord::Base
  belongs_to :analyst
  belongs_to :analyst_token

  CA_PATH = "/aircloak/ca"

  def self.ca_key_file(type)
    File.join(CA_PATH, "#{type}.key")
  end

  def self.ca_cert_file(type)
    File.join(CA_PATH, "#{type}.cert")
  end

  def self.api_ca
    [File.read(ca_key_file("api")), File.read(ca_cert_file("api"))]
  end

  # A key_material contains the following properties
  # - certificate: the raw certificate
  # - key: the raw key protected with the aircloak password
  # - pkcs12: a packaged version of the unprotected key and the certificate
  #     protected by a password chosen by the analyst. Not usable by aircloak.
  #     Aircloak can regenerate a pkcs12 based on the raw certificate
  #     and key.

  # We currently only support keys for
  # - uploading data for any user
  # - inquiring (i.e. running tasks etc)
  #
  # Currently we also only allow downloads of the keys in pkcs12
  # format. This turns out to be somewhat problematic, and should
  # be extended to include regular pem's as well.
  #
  # Keys for uploading data for an individual user will be added
  # on demand at a later time.

  # Creates a new key, cert pair signed by the analysts master key.
  # A corresponding pkcs12 protected by a password specified by the
  # analyst is also generated.
  def self.create_from_analyst analyst, password, description, key_type
    analyst_token = nil
    key_material = analyst.key_materials.new
    key_material.key_type = key_type

    # As far as IDs in the keys go, we use 1 for general purpose exported keys,
    # with the exception of the data_upload_all key, where it is the convention
    # that 0 signifies that the key can be used to uplad data for any user.
    case key_material.key_type
    when "data_upload_all"
      key_description = "#{description}. Allows uploading data for any user to any of #{analyst.name}'s clusters"
      raw_key, raw_cert = TokenGenerator.generate_leaf_token analyst.key, analyst.certificate, "any_user", 0

    when "admin"
      key_description = "#{description}. Allows performing administrative tasks against any of #{analyst.name}'s clusters"
      raw_key, raw_cert = TokenGenerator.generate_leaf_token analyst.key, analyst.certificate, "admin", 1

    when "task_runner"
      key_description = "#{description}. Allows executing tasks against any of #{analyst.name}'s clusters"
      raw_key, raw_cert = TokenGenerator.generate_leaf_token analyst.key, analyst.certificate, "task_runner", 1

    when "web_api"
      key_description = "#{description}. Allows issuing REST API calls to web"
      api_key, api_cert = api_ca
      analyst_token = AnalystToken.create_api_token(analyst)
      raw_key, raw_cert = TokenGenerator.generate_leaf_token api_key, api_cert, "analyst_token", analyst_token.token
    end

    key = TokenGenerator.import_key raw_key
    cert = OpenSSL::X509::Certificate.new raw_cert

    pkcs12 = OpenSSL::PKCS12.create(
      password,
      key_description,
      key,
      cert
    )
    key_material.pkcs12 = Base64.encode64 pkcs12.to_der
    key_material.pem = key.to_pem(OpenSSL::Cipher.new('AES-256-CBC'), password) + cert.to_pem
    key_material.key = raw_key
    key_material.certificate = raw_cert
    key_material.description = description
    key_material.analyst_token = analyst_token unless analyst_token.nil?
    key_material.save
    key_material
  end

  def name format
    "#{analyst.name}_#{id}.#{format}"
  end

  # Used for category headings in the view
  def self.type_description type
    case type
    when "admin"
      "Administrative keys"
    when "data_upload_all"
      "Upload data for all users keys"
    when "task_runner"
      "Keys for running tasks"
    when "web_api"
      "Keys for issuing REST API calls to web"
    end
  end
end
