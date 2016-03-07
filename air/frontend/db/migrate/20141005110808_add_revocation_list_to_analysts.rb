class AddRevocationListToAnalysts < ActiveRecord::Migration
  def change
    add_column :analysts, :revocation_list, :text
    # generate empty revocation list for existing analysts
    Analyst.all.each do |analyst|
      analyst.revocation_list = TokenGenerator.generate_empty_revocation_list(analyst.key, analyst.certificate)
      analyst.save
    end
  end
end
