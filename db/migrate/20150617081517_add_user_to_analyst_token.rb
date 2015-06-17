class AddUserToAnalystToken < ActiveRecord::Migration
  def change
    add_reference :analyst_tokens, :user, index: true
    # for existing tokens, assign the first user of the analyst as the owner
    AnalystToken.all.each do |token|
      token.user = token.analyst.users.first
      token.save!
    end
  end
end
