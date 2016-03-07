class RemoveOrphanedLogMessages < ActiveRecord::Migration
  def change
    AuditLog.remove_orphaned_logs
  end
end
