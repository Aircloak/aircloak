class AddInquirerTokenToAnalyst < ActiveRecord::Migration
  def change
    add_column :analysts, :inquirer_key, :text
    add_column :analysts, :inquirer_cert, :text

    Analyst.all.each do |analyst|
      analyst.send :create_inquirer_token
      analyst.save
    end
  end
end
