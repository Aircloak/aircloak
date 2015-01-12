class RemoveOrphanedRepeatedAnswers < ActiveRecord::Migration
  def change
    RepeatedAnswer.remove_orphaned_reports
  end
end
