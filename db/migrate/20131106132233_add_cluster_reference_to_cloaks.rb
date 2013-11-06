class AddClusterReferenceToCloaks < ActiveRecord::Migration
  def change
    add_reference :cloaks, :cluster, index: true
  end
end
