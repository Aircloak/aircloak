class AddClusterReferenceToCloaks < ActiveRecord::Migration
  def change
    add_belongs_to :cloaks, :cluster, index: true
  end
end
