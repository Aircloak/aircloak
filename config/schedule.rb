every 1.day, :at => '0:30 am' do
  runner "VersionTest.new_from_develop_branch"
end
