Central stats
=============

Contains the source code for the R-dashboard that is used to show usage statistics in the Central stats section.
It is hosted as a [Shiny server](https://www.rstudio.com/products/shiny/shiny-server/) on `srv-76-133`.

In order to deploy, copy the relevant R and Rmd files to `srv-76-133:/srv/shiny-server/stats/`.
A convenience script has been added for this. It can be invoked with `make deploy`.

# Local development

If you want to test and develop locally, make sure to create a copy of the `credentials.R.sample` file called
`credentials.R`. Git ignores it, so you can keep it around in your working directory. The credentials should be for
a user at [demo.aircloak.com](https://demo.aircloak.com/) with access the the [Central stats](https://demo.aircloak.com/data_sources/CentralStats) data source.

# R packages

If you add and use new packages which are not part of a standard R distribution, then you will have to SSH into
`srv-76-133` and manually install the packages in R there.
