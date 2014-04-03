authorization do
  role :guest do
    has_permission_on :welcome, to: :index
    has_permission_on :get_latest, to: :show
    has_permission_on :user_sessions, to: [:new, :create, :destroy]
    has_permission_on :users, to: [:show, :create, :update] do
      if_attribute :user => is {user}
    end
    has_permission_on [
      :commands,
      :client_binaries,
      :queries,
      :machines,
      :cluster_lists,
      :api_queries
    ], to: :anon_read
    has_permission_on [
      :verifications,
      :results,
      :register_version,
      :build_progress,
      :machines,
      :tasks,
      :version_tests,
      :api_queries,
      :api_clusters
    ], to: :anon_write
  end

  role :ops do
    includes :guest
    has_permission_on [:staging_machines, :deployment_groups], to: :manage
  end

  role :client_manager do
    includes :guest
    has_permission_on [:client_file_versions, :client_binaries, :client_files, :client_file_types], to: :manage
    has_permission_on [:commands, :deployment_groups, :verifications], to: :read
  end

  role :deploy_manager do
    includes :guest
    includes :client_manager
    has_permission_on [:verifications, :commands, :command_file_versions], to: :manage
    has_permission_on :deployment_groups, to: :create_command
  end

  role :admin do
    includes [:ops, :deploy_manager]
    has_permission_on [:queries,
                       :tasks,
                       :results,
                       :users,
                       :deployable_entities,
                       :deployable_entity_versions,
                       :builds,
                       :clusters,
                       :cloaks,
                       :permissions,
                       :os_tags,
                       :version_tests,
                       :user_permissions,
                       :metrics], to: :manage
    has_permission_on [:authorization_rules,
                       :authorization_usages], :to => :read
    has_permission_on [:deployable_entity_versions,
                       :builds], to: :reset
  end

  # role :user do
  #   includes :guest
  #   has_permission_on :conference_attendees, :to => :create, :join_by => :and do
  #     if_attribute :user => is {user}
  #     if_permitted_to :read, :conference
  #   end
  #   has_permission_on :conference_attendees, :to => :delete do
  #     if_attribute :user => is {user}
  #   end
  #   has_permission_on :talk_attendees, :to => :create do
  #     if_attribute :talk => { :conference => { :attendees => contains {user} }},
  #         :user => is {user}
  #   end
  #   has_permission_on :talk_attendees, :to => :delete do
  #     if_attribute :user => is {user}
  #   end
  # end

  # role :conference_organizer do
  #   has_permission_on :conferences do
  #     to :manage
  #     # if...
  #   end
  #   has_permission_on [:conference_attendees, :talks, :talk_attendees], :to => :manage
  # end
end

privileges do
  privilege :manage, :includes => [:create, :read, :update, :delete]
  privilege :read, :includes => [:index, :show]
  privilege :create, :includes => :new
  privilege :update, :includes => :edit
  privilege :delete, :includes => :destroy
  privilege :anon_read
  privilege :anon_write
end
