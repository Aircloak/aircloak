require 'vcr'

VCR.configure do |c|
  c.cassette_library_dir = 'spec/fixtures/vcr_cassettes'
  c.hook_into :excon
end

module PreRecorded
  def self.repo_erlattest
    
  end

  def self.erlattest_commit block
    VCR.use_cassette('erlattest-commit-message', allow_playback_repeats: true) do
      yield block
    end
  end
end
