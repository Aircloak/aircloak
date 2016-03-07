class RemoveUnusedDeployableEntityVersions < ActiveRecord::Migration
  def change
    # This clears out all deployable entity versions which are
    # not part of a build anyway. Since we no longer choose a
    # version from a predefined list of versions, we might just
    # as well remove these stale ones once and for all.
    DeployableEntityVersion.all.each do |dev|
      dev.destroy if dev.can_destroy?
    end
  end
end
