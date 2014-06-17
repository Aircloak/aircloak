require 'securerandom'
require 'openssl'
require 'socket'

# Methods for generating authentication tokens. A token is comprised of a private key and a signed certificate.

# NOTE: This is security sensitive code!
# Please do not do any modifications to it without good reason and extensively reading the documentation first.

# We have 2 types of tokens that can be generated:
# 1. Root Entity Token: this is the root certificate authority (CA) for a trust chain and can sign
#     other leaf tokens with it's private key.
# 3. Leaf Entity Token: this token can only be used for authentication with a cloak on behalf of
#     the root entity and cannot sign other tokens.

module TokenGenerator
  def self.generate_random_string_of_at_least_length length
    return SecureRandom.urlsafe_base64(length)
  end

  CRITICAL = true
  NON_CRITICAL = false

  def self.generate_subject_string(type, id)
    return "/C=DE/O=AircloakUG/L=Kaiserslautern/CN=#{type}:#{id}/emailAddress=contact@aircloak.com"
  end

  def self.generate_certificate_serial_number()
    return 1 + Random.rand(2**128 - 2)
  end

  def self.generate_private_key()
    return OpenSSL::PKey::RSA.new(2048)
  end

  def self.export_key(key)
    # This is used as a small protection against database leaks without access to server files.
    privateKeyPassword = Rails.configuration.private_key_password
    abort "private_key_password field is not set in the settings.local.yml file." unless privateKeyPassword
    return key.export(OpenSSL::Cipher.new('AES-256-CBC'), privateKeyPassword)
  end

  def self.import_key(keyText)
    # This is used as a small protection against database leaks without access to server files.
    privateKeyPassword = Rails.configuration.private_key_password
    abort "private_key_password field is not set in the settings.local.yml file." unless privateKeyPassword
    return OpenSSL::PKey::RSA.new(keyText, privateKeyPassword)
  end

  def self.decrypt_key_text(keyText)
    return import_key(keyText).export()
  end

  def self.generate_root_token(entityType, entityId)
    rootKey = generate_private_key()

    rootCertificate = OpenSSL::X509::Certificate.new
    rootCertificate.subject = OpenSSL::X509::Name.parse(generate_subject_string(entityType, entityId))
    rootCertificate.issuer = rootCertificate.subject # root CA's are "self-signed"
    rootCertificate.not_before = Time.now
    rootCertificate.not_after = Time.now + 100 * 365 * 24 * 60 * 60 # valid for 100 years
    rootCertificate.public_key = rootKey.public_key
    rootCertificate.serial = generate_certificate_serial_number()
    rootCertificate.version = 2 # cf. RFC 5280 - to make it a "v3" certificate

    rootEF = OpenSSL::X509::ExtensionFactory.new
    rootEF.subject_certificate = rootEF.issuer_certificate = rootCertificate

    # Mark as certificate authority.
    rootCertificate.add_extension(rootEF.create_extension("basicConstraints", "CA:TRUE,pathlen:1", CRITICAL))
    rootCertificate.add_extension(rootEF.create_extension("keyUsage", "digitalSignature,cRLSign,keyCertSign", CRITICAL))
    rootCertificate.add_extension(rootEF.create_extension("subjectKeyIdentifier", "hash", NON_CRITICAL))
    rootCertificate.add_extension(rootEF.create_extension("authorityKeyIdentifier", "keyid:always,issuer:always", NON_CRITICAL))

    # TODO (Aircloak/web#265): uncomment this when CRL endpoints are implemented
    # Set certificate revokation list location. Note: verification may fail when they are unavailable.
    #hostname = Socket.gethostbyname(Socket.gethostname).first
    #rootCertificate.add_extension(rootEF.create_extension("crlDistributionPoints", 
    #  "URI:https://#{hostname}/crls/cluster/#{clusterId}/revoked.pem", CRITICAL))

    rootCertificate.sign(rootKey, OpenSSL::Digest::SHA256.new)

    return export_key(rootKey), rootCertificate.to_pem()
  end

  def self.generate_leaf_token(caKeyText, caCertificateText, entityType, entityId)
    entityKey = generate_private_key()

    caKey = import_key(caKeyText)
    caCertificate = OpenSSL::X509::Certificate.new(caCertificateText)

    entityCertificate = OpenSSL::X509::Certificate.new
    entityCertificate.version = 2 # cf. RFC 5280 - to make it a "v3" certificate
    entityCertificate.serial = generate_certificate_serial_number()
    entityCertificate.subject = OpenSSL::X509::Name.parse(generate_subject_string(entityType, entityId))
    entityCertificate.issuer = caCertificate.subject # CA is the issuer
    entityCertificate.public_key = entityKey.public_key
    entityCertificate.not_before = Time.now
    entityCertificate.not_after = entityCertificate.not_before + 100 * 365 * 24 * 60 * 60 # 100 years validity

    entityEF = OpenSSL::X509::ExtensionFactory.new
    entityEF.subject_certificate = entityCertificate
    entityEF.issuer_certificate = caCertificate

    # Mark as endpoint certificate.
    entityCertificate.add_extension(entityEF.create_extension("basicConstraints", "CA:FALSE", CRITICAL))
    entityCertificate.add_extension(entityEF.create_extension("keyUsage", "digitalSignature,keyEncipherment", CRITICAL))
    entityCertificate.add_extension(entityEF.create_extension("subjectKeyIdentifier", "hash", NON_CRITICAL))

    entityCertificate.sign(caKey, OpenSSL::Digest::SHA256.new)

    return export_key(entityKey), entityCertificate.to_pem()
  end
end
