require 'digest/sha1'

class FingerPrintCreator
  def self.create_sha something
    Digest::SHA1.base64digest something
  end

  def self.fingerprint build
    versions = build.deployable_entity_versions
    versions.sort! do |a,b|
      a.commit_id <=> b.commit_id
    end
    create_sha "#{build.tpm}-#{versions.map(&:commit_id).join("-")}"
  end
end
