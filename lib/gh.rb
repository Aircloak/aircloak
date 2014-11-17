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

  def self.buildpoint_for_deployable_entity de
    build_infos = []
    branch_thread = Thread.new do
      threads = []
      branches = github.repos.branches user: "aircloak", repo: de.repo
      branches.each do |branch|
        threads << Thread.new do
          commit = get_commit de.repo, branch.commit.sha
          build_infos << {
            name: branch.name,
            sha: branch.commit.sha,
            title: commit.commit.message.split("\n").first
          }
        end
      end
      threads.each {|thread| thread.join}
    end
    tag_thread = Thread.new do
      tags = github.repos.tags user: "aircloak", repo: de.repo
      tags.each do |tag|
        build_infos << {
          name: tag.name,
          sha: tag.commit.sha,
          title: "release tag"
        }
      end
    end
    branch_thread.join
    tag_thread.join
    build_infos
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
