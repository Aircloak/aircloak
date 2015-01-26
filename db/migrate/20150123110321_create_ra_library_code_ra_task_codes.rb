class CreateRaLibraryCodeRaTaskCodes < ActiveRecord::Migration
  def change
    create_table :ra_library_code_ra_task_codes do |t|
      t.belongs_to :ra_library_code, index: true
      t.belongs_to :ra_task_code, index: true

      t.string :name

      t.timestamps
    end
  end
end
