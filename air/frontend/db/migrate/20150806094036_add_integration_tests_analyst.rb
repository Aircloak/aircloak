require './lib/token_generator'

class AddIntegrationTestsAnalyst < ActiveRecord::Migration
  def change
    # If a completely fresh migration takes place from an empty database,
    # then the User class we have at this point does not yet have a notion
    # of analysts! We have to ensure it does, otherwise this migration won't work!
    User.reset_column_information
    Permission.reset_column_information

    analyst_name = "Aircloak - Integration tests"
    user_login = "infrastructure-test"

    reversible do |dir|
      dir.up do
        analyst = Analyst.create(name: analyst_name)
        random_password = TokenGenerator.generate_random_string_of_at_least_length(50)
        analyst.users.create(
          login: user_login,
          email: "infrastructure-test@aircloak.com",
          password: random_password,
          password_confirmation: random_password,
          permission_ids: [Permission.find_by_name("admin").id]
        )
      end

      dir.down do
        User.find_by_login(user_login).destroy
        Analyst.find_by_name(analyst_name).destroy
      end
    end
  end
end
