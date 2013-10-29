require 'github_api'

class Gh
  def self.description_for repo
    github.repos.find("aircloak", repo).description
  rescue Github::Error::NotFound
    raise UnknownRepository
  end

private
  def self.github
    @github ||= Github.new do |config|
      config.oauth_token = '273c13ccc918a992618a503df7dadfe06f70b3f5'
      config.adapter     = :excon
      config.ssl         = {:verify => true}
    end
  end
end

class UnknownRepository < Exception
end
