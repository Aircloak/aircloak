class CreateCommands < ActiveRecord::Migration
  def change
    create_table :commands do |t|
      t.binary :command_binary
      t.boolean :valid_command, default: false
      t.integer :times_downloaded, default: 0

      t.timestamps
    end
  end
end
