## Important notice

Docker allows restricting the amount of memory made available to a given docker container. __Do not use this form of
restriction on Insights Cloak instances__. It interferes with Insights Cloak's ability to detect and terminate rampant queries, and
might result in the component becoming unavailable.

If restricting the amount of memory made available to Insights Cloak is desired, it is recommended to deploy Insights
Cloak in a separate virtual machine.
