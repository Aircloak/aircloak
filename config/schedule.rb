# Use this file to easily define all of your cron jobs.
# Learn more: http://github.com/javan/whenever

set :output, "log/cron.log"

every :day do
  runner "Task.purge_deleted_tasks"
end
