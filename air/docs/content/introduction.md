# Introduction

This is the guide for using the Aircloak system.

If you find some topics are missing or confusing, please reach out to us on [support@aircloak.com](mailto:support@aircloak.com), and we will be happy to assist.

## Reading guide

### Analysts and data scientists

If you are an __analyst__ or __data scientist__, we recommend the following sections:

- The [core language features](sql.md) chapter gives an overview of the subset of SQL supported by Aircloak Insights.
- The [best practises](sql/best-practises.md) chapter walks through some important considerations when it comes to working
  with Aircloak Insights as an analyst.
- [Understanding query results](sql/query-results.md) explains the effects of anonymisation on the results you are
  receiving and how these can be quantified.

### System administrators

If you are a __system administrator__ who wants to install and configure the Aircloak Insights platform, we recommend
the following sections:

- The [components of Aircloak Insights](components.md) gives you an initial understanding of what components the
  system is made out of and how they interact.
- The [resource requirements](deployment.md#resource-requirements) section of the deployment guide gives an idea of
  the resources you should make available to the distinct components of the system.
- The [installation guide](ops/installation.md) in the operations section guides you through the installation.
- Once Aircloak Insights is running, you will want to consult the [monitoring](ops/monitoring.md) section for information
  on how to integrate Aircloak Insights into your monitoring system. This ensures you get notified if components are
  misbehaving or the system needs your attention.
