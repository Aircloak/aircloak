class ChangeStringToTextType < ActiveRecord::Migration
  def change
    # The string type is varchar 255, and log outputs and commit messages
    # are very likely to exceed this limit. This causes 500 errors to be
    # thrown.
    change_column :deployable_entity_versions, :message, :text
    change_column :deployable_entity_versions, :build_log_tpm, :text
    change_column :deployable_entity_versions, :build_log_no_tpm, :text

    # A stack trace can easily exceed 255 characters.
    # We don't want errors thrown when receiving inputs from the cloaks,
    # so changing type to text
    change_column :exception_results, :stacktrace, :text

    # A test result, in all likelihood will exceed 255 characters as well.
    change_column :version_tests, :test_output, :text
  end
end
