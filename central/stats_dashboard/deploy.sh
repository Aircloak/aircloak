echo "Deploying R files to Shiny server"
scp main.Rmd srv-76-133:/srv/shiny-server/stats/
scp queries.R srv-76-133:/srv/shiny-server/stats/
