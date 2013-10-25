class AddDefaultUser < ActiveRecord::Migration
  def change
    u = User.create(
      login: "test",
      email: "doesnt-exist@aircloak.com",
      password: "1234",
      password_confirmation: "1234"
    )
    u.permissions << Permission.where(name: "admin")
  end
end
