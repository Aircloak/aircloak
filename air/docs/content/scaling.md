# Scaling Aircloak Insights

## Low query volume

### Single datasource

The most common setup is one where the customer has a single datasource that they want to give analysts access to. This
datasource might be hosted across a single or multiple database servers. If the query volume is low, a single Insights
Cloak instance is sufficient.

![Shows the base level Aircloak Insights setup. IA: Insights Air, IC: Insights
Cloak, IDC: Insights Datasource connector, DS: Datasource](scaling/single-low.png)


### Multiple datasources

When an organisation has many datasources that should be made available for querying but a low number of concurrent
queries, a single Insight Cloak instance can be configured to serve them all. The datasources can be located in a single
or on multiple different database servers.

![Shows a setup of Aircloak Insights where multiple data sources are connected to a single Insights Cloak. IA: Insights Air, IC: Insights
Cloak, IDC: Insights Datasource connector, DS: Datasource](scaling/multi-low.png)


## High query volume

### Single datasource

When a high volume of concurrent queries are issued to one and the same datasource, the number of Insight Cloak and
Insight Database Connector instances can be scaled up. When the same datasource is made available by different Insight
Cloak instances, Insights Air will spread the queries across the Insight Cloak instances. While a given query will only
run on a single Insights Cloak instance at a time, additional queries can be executed on other available Insights Cloak
instances without affecting its performance.

![Shows a setup of Aircloak Insights allowing higher number of concurrent queries. IA: Insights Air, IC: Insights
Cloak, IDC: Insights Datasource connector, DS: Datasource](scaling/single-high.png)


### Multiple datasources

When multiple datasources receive a high volume of queries there are two ways in which one can approach the
setup.

The first approach is conceptually the simplest. It uses multiple individual instances of the
[single datasource, high query volume](#high-query-volume) setup. These can connect to
a single Insights Air, or each be given a dedicated Insight Air instance.

The second approach is to configure multiple Insight Cloak instances all serving all available datasources. This
approach is shown in the diagram below. This model works well when the number of queries issued to the different
datasources vary over time. As the hardware resources are shared across all datasources it can accommodate for sudden
spikes in the query load for a given datasource, but since the hardware resources are shared the query performance
across the datasources varies over time depending on the overall load.

![Shows a setup of Aircloak Insights load balancing a higher number of queries across multipel data sources. IA: Insights Air, IC: Insights
Cloak, IDC: Insights Datasource connector, DS: Datasource](scaling/multi-high.png)
