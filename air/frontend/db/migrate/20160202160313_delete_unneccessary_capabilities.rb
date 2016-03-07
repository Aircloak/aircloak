class DeleteUnneccessaryCapabilities < ActiveRecord::Migration
  # We don't do an up/down migration here for the following reasons:
  # - when destroying the capabilities, we loose the mappings
  #   of which capability belongs to which cluster.
  #   This is something we cannot recreate
  def change
    capabilities = [
      "lua_library_support",
      "postgres_text_column_support",
      "user_row_expiry",
      "task_progress_reports",
      "task_select_columns"
    ]
    capabilities.each do |identifier|
      capability = Capability.find_by_identifier(identifier)
      capability.destroy if capability
    end
  end
end
