require 'github_api'

class Gh
  def self.description_for repo
    github.repos.find("aircloak", repo).description
  rescue Github::Error::NotFound
    raise UnknownRepository
  end

  def self.add_message_and_author version
    repo = version.deployable_entity.repo
    commit_id = version.commit_id
    commit = github.repos.commits.find("aircloak", repo, commit_id).commit
    version.message = commit.message
    version.author = commit.author.name
  end

  def self.latest_commit_on_branch_for_repo branch, repo
    commits = github.repos.commits.list "aircloak", repo, sha: branch
    commits.first.sha
  end

private
  def self.github
    raise "Missing Github oauth token in settings.local.yml" unless Rails.configuration.respond_to? :github_oauth_token
    @github ||= Github.new do |config|
      config.oauth_token = Rails.configuration.github_oauth_token
      config.adapter     = :excon
      config.ssl         = {:verify => true}
    end
  end
end

class UnknownRepository < Exception
end
