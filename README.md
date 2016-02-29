Aircloak
========

This repository contains the entirety of the Aircloak system.
A system that allows databases to be queried while enforcing
the anonymity of the individuals in the dataset.

There are two main components:

- __air__: a central control point only operating on non-sensitive data
- __cloak__: a component deployed close to the data, performing the querying and anonymization
