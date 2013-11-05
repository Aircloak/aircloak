require './lib/finger_print_creator.rb'

describe FingerPrintCreator do
  def create_version i
    double("#version_#{i}".to_sym, commit_id: FingerPrintCreator.create_sha(i.to_s))
  end

  def build_for versions
    double(tpm: true, deployable_entity_versions: versions)
  end

  it "should be able to sha something" do
    FingerPrintCreator.create_sha("something").should eq "GvF+c3IdvgxAARuC7Uuxp9vjzik="
  end

  it "should create consistent fingerprints" do
    versions = []
    3.times do |i|
      versions << create_version(i)
    end

    build1 = build_for versions
    fp1 = FingerPrintCreator.fingerprint build1

    build2 = build_for versions.reverse
    fp2 = FingerPrintCreator.fingerprint build2

    versions.pop
    versions << create_version(4)
    build3 = build_for versions

    fp3 = FingerPrintCreator.fingerprint build3

    fp1.should_not eq nil
    fp1.should eq fp2
    fp1.should_not eq fp3
  end

  it "should create fingerprints that depend on whether its for TPM or not" do
    build1 = double(deployable_entity_versions: [], tpm: true)
    build2 = double(deployable_entity_versions: [], tpm: false)
    
    fp1 = FingerPrintCreator.fingerprint build1
    fp2 = FingerPrintCreator.fingerprint build2
    fp1.should_not eq fp2
  end
end
