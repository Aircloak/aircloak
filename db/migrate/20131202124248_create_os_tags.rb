class CreateOsTags < ActiveRecord::Migration
  def change
    create_table :os_tags do |t|
      t.string :name
      t.string :description

      t.timestamps
    end
  end
end
