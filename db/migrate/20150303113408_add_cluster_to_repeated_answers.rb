class AddClusterToRepeatedAnswers < ActiveRecord::Migration
  def change
    add_belongs_to :repeated_answers, :cluster, index: true
  end
end
