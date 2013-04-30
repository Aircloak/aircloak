# This file should contain all the record creation needed to seed the database with its default values.
# The data can then be loaded with the rake db:seed (or created alongside the db with db:setup).
#
# Examples:
#
#   cities = City.create([{ name: 'Chicago' }, { name: 'Copenhagen' }])
#   Mayor.create(name: 'Emanuel', city: cities.first)

Index.create(human_name: "All users", name: "all_users", system_index: true)

Permission.create(name: "ops", description: "Allowed to manage staging machines and deployment groups")
admin = Permission.create(name: "admin", description: "Super user with rights to do everything")
Permission.create(name: "client_manager", description: "Can upload new versions of the client binary")
Permission.create(name: "deploy_manager", description: "Can push changes to deployment groups")

u = User.new(login: "seb", email: "sebastian@aircloak.com", password: "1234", password_confirmation: "1234")
u.permissions << admin
u.save
