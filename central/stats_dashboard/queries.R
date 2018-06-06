library(parallel)

queries = list(
  queriesByDay="
    SELECT
      DATE_TRUNC('month', aux.started_at) as queryTime,
      count(*) as numQueries,
      count(distinct customer.name) as distinctCustomers
    FROM queries
    GROUP BY 1
    ORDER BY 1 ASC
  ",

  queriesByDayAndCustomer="
    SELECT
      customer.name as customerName,
      1 as distinctCustomers,
      DATE_TRUNC('month', aux.started_at) as queryTime,
      count(*) as numQueries
    FROM queries
    GROUP BY 1, 2, 3
    ORDER BY 3 ASC
  ",

  queriesByTimeOfDay="
    SELECT
      hour(aux.started_at) as hour,
      count(*) as numQueries,
      count(distinct customer.name) as distinctCustomers
    FROM queries
    GROUP BY 1
    ORDER BY 1 ASC
  ",

  queriesByTimeOfDayAndCustomer="
    SELECT
      customer.name as customerName,
      1 as distinctCustomers,
      hour(aux.started_at) as hour,
      count(*) as numQueries
    FROM queries
    GROUP BY 1, 2, 3
    ORDER BY 3 ASC
  ",

  queriesByDayOfMonth="
    SELECT
      day(aux.started_at) as day,
      count(*) as numQueries,
      count(distinct customer.name) as distinctCustomers
    FROM queries
    GROUP BY 1
    ORDER BY 1 ASC
  ",

  queriesByDayOfMonthAndCustomer="
    SELECT
      customer.name as customerName,
      1 as distinctCustomers,
      day(aux.started_at) as day,
      count(*) as numQueries
    FROM queries
    GROUP BY 1, 2, 3
    ORDER BY 3 ASC
  ",

  totalQueryCount="
    SELECT
      'All customers' as customerName,
      count(*) as numQueries
    FROM queries
  ",

  totalQueryCountByCustomer="
    SELECT
      customer.name as customerName,
      count(*) as numQueries
    FROM queries
    GROUP BY 1
  ",

  avgQueriesByPeriod="
    SELECT
      'allCustomers' as customerName,
      avg(totalByPeriod) as avg
    FROM (
      SELECT
        period,
        SUM(countByPeriod) as totalByPeriod
      FROM (
        SELECT
          user_id,
          date_trunc('month', aux.finished_at) as period,
          count(*) as countByPeriod
        FROM queries
        GROUP BY 1, 2
      ) queries_per_period_per_analyst
      GROUP BY 1
    ) queries_per_period
    GROUP BY 1
  ",

  avgQueriesByPeriodAndCustomer="
    SELECT
      customerName,
      avg(totalCountByPeriod) as avg
    FROM (
      SELECT
        period,
        customerName,
        SUM(countByPeriod) as totalCountByPeriod
      FROM (
        SELECT
          user_id,
          customer.name as customerName,
          date_trunc('month', aux.started_at) as period,
          count(*) as countByPeriod
        FROM queries
        GROUP BY 1, 2, 3
      ) queries_per_period_per_analyst
      GROUP BY 1, 2
    ) queries_per_period
    GROUP BY 1
  ",

  queryExecutionTimes="
    SELECT
      'All customers' as customerName,
      bucket(metrics.execution_time by 1 align middle) as executionTime,
      count(*)
    FROM queries
    GROUP BY 1, 2
    ORDER BY 2 ASC
  ",

  queryExecutionTimesByCustomer="
    SELECT
      customer.name as customerName,
      bucket(metrics.execution_time by 1 align middle) as executionTime,
      count(*)
    FROM queries
    GROUP BY 1, 2
    ORDER BY 2 ASC
  ",

  dataSources="
    SELECT
      features.driver as driver,
      count(*)
    FROM queries
    GROUP BY 1
    ORDER BY count(*) DESC
  ",

  dataSourcesByCustomer="
    SELECT
      customer.name as customerName,
      features.driver as driver,
      count(*)
    FROM queries
    GROUP BY 1, 2
    ORDER BY customerName, count(*) DESC
  ",

  distinctCustomers="
    SELECT customer.name as customerName
    FROM queries
    GROUP BY 1
  ")

getAndPrepData <- function(query) {
  library("RPostgreSQL")
  drv <- dbDriver("PostgreSQL")
  source(file="credentials.R")
  con <- dbConnect(drv, dbname = "CentralStats",
                   host = "demo.aircloak.com", port = 8432,
                   user = dbUsername, password = dbPassword)
  data <- dbGetQuery(con, query)
  dbDisconnect(con)
  data[complete.cases(data),]
}

concurrency <- min(detectCores(), 10)
cluster <- makeCluster(concurrency)
queryResults <- parLapply(cluster, queries, getAndPrepData)
stopCluster(cluster)
