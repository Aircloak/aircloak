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

  def self.branchinfo_for_deployable_entity de
    branches = github.repos.branches user: "aircloak", repo: de.repo
    branch_infos = []
    threads = []
    branches.each do |branch|
      threads << Thread.new do
        commit = get_commit de.repo, branch.commit.sha
        branch_infos << {
          branch_name: branch.name,
          sha: branch.commit.sha,
          commit_title: commit.commit.message.split("\n").first
        }
      end
    end
    threads.each {|thread| thread.join}
    branch_infos
  end

  def self.get_commit repo, sha
    github.repos.commits.get user: "aircloak", repo: repo, sha: sha
  end

  def self.github
    @github ||= Github.new do |config|
      config.oauth_token = Rails.configuration.github_oauth_token
      config.adapter     = :excon
      config.ssl         = {:verify => true}
    end
  end
end

class UnknownRepository < Exception
end
