class AuditLog < ActiveRecord::Base
  belongs_to :cloak

  validates_presence_of :cloak_id, :log_message, :log_id

  def self.create_from_log_message entry, cloak_id
    signature, hash, id = entry.split ";"
    AuditLog.new log_id: id, log_message: entry, cloak_id: cloak_id
  end

  def signature
    signature = message_parts[0]
    signature == "" ? "<UNSIGNED>" : signature
  end

  def hash
    message_parts[1]
  end

  def parent_hash
    parent_entry = AuditLog.where cloak_id: cloak_id, log_id: (log_id - 1)
    if parent_entry.first
      parent_entry.first.hash
    else
      "<NO PARENT ENTRY AVAILABLE>"
    end
  end

  def verified
    true
  end

  def source
    message_parts[3]
  end

  def timestamp
    message_parts[4]
  end

  def message
    message_parts[5..(message_parts.size)].join(";").gsub("\\n", "\n")
  end

  # Removes all repeated answer for which there is no longer
  # an analyst. These are dangling reports from earlier times
  # before we destroyed reports along with the analysts.
  def self.remove_orphaned_logs
    sql = "DELETE FROM audit_logs WHERE cloak_id not in (SELECT id from cloaks)"
    ActiveRecord::Base.connection.execute(sql)
  end

private
  def message_parts
    @parts ||= log_message.split ";"
  end
end
