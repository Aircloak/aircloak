Changelog
=========

This change log gives a high level overview of what has changed
between versions. It can serve as an initial guide to explain
the purpose of a release, and help guide auditors when reviewing the code.

---------------

# Versions

## develop

## sasa/enhancement/db_pool

- Implement unlimited database connection pool
- Remove poolboy as a dependency

### matthias/enhancement/ring_manager-splitup merge

- The merge introduces a new on-disk format for the current active ring and other persisted information.
- New API for testing if the local node is online or ready via module `node_state`.
- New API for current active and next rings via module `ring_state`.
- New API for managing joining and leaving nodes via module `joinleave_manager`.
- The module `ring_manager` is no more.

### sasa/enhancement/remove-global-db-timeout

- Remove global timeout (lease time) when acquiring connections
- Support per-query timeout (default 20s) and enforce finite timeout values
- Remove gen_pool and gen_pool_worker
- If the client process which has the connection crashes, the connection is closed.


## 1.0.0

Marks first release for review aimed at becoming a production release.

- Remove log messages which might cause privacy leak
- Use simplified cloak-metrics without gauges to prevent private data from leaking
- Never report negative counts
- Log on excessive resource and disk usage for capacity planning
- Cap SD to 20 for random noise in answers
- Allow tasks to determine the return URL where results are sent
- Remove `/ping` endpoint
- Namespace aircloak database tables and columns with `ac_`
- Add notion of ring for data migration and basic syncing to ensure that nodes which
  have been temporarily down get copies of all data they are expected to hold
- Add auditor guidelines


## 0.0.0

Initial release providing a fixed point that can be used by auditors when
attesting and validating the boot and attestation process.

This version is __not suitable for production use__.
