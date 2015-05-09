class CreateAuditLogs < ActiveRecord::Migration
  def change
    create_table :audit_logs do |t|
      t.integer :cloak_id
      t.text :log_message
      t.integer :log_id

      t.timestamps
    end
  end
end
