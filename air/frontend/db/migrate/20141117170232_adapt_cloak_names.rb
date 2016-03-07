class AdaptCloakNames < ActiveRecord::Migration
  def change
    Cloak.all.each do |cloak|
      if (cloak.name =~ /([\w\d-]+)\.mpi-sws\.org/) == 0 then
        cloak.name = $1
        cloak.save
      end
    end
  end
end
