airpub application
==========

----------------------

- [What it does](#what-it-does)
- [Design](#design)
- [Getting started](#getting-started)

----------------------


# What it does

Airpub is a distribution centre for anonymized metrics and results.

cloak-core sends data to airpub, which in turn persists the data to the database.
Analysts can also subscribe to particular queries in airpub, and get data sent directly
as it becomes available. This is necessary for streaming queries, but also extremely useful
for batch queries.


## Design

The main attributes prioritized during the design of the airpub system were:

  - reduced latency - we want articles to reach subscribers as fast as possible.
  - security - we only want authorized by web subscribers to be able to receive notifications and only cloak-core to publish data.
  - abstracted subscription mechanism - we should be able to subscribe to certain nodes using multiple mechanisms: http requests, web-sockets or plain TCP connections.
  - stateless execution - it should result in lower maintenance overhead and simpler design to have a system that doesn't need to store state on disk between executions (less things to go wrong, less room for failures).

Although systems like Ejabberd or Redis have proven pubsub features that scale to lots of concurrent users, our usage model doesn't map 100% over them and bolting all our needed features on top of them is not ideal, as we would soon start to add lots of hacks so that we bend them to our needs. The decision was made to have a simple Erlang pubsub system that we fully control.

The current system provides web-socket support for subscribing to notifications and an HTTP endpoint for cloak-core to publish data.
The publish endpoint will mirror the web endpoint so that cloak-core will not care if it talks with airpub or web. The endpoint is secured by using IP filtering in the nginx config, but we can also add client certificate support if needed.
All subscribtion requests need to be authorized by the web component. In order to reduce latency and overhead, web and airpub share a secret key used for signing and authenticating the requests, without the need of active communication between the two systems. All subscriptions signatures expire after a fixed period of time (this requires web and airpub to have synchronized clocks).
The system doesn't save any data on disk. Subscription nodes do not have to be pre-created for clients to publish or subscribe to them. All routing is done dynamically as requests come and go. The system will keep a history of published articles that will get sent to new subscribers, so there is no need to have the subscription executed before the article arrives, as long as the window between the event is short enough (which is most often the case).


## Getting started

The routed data is represented internally as an article record. It has a content field (opaque, binary data), a content type (optional, usually MIME type) and publication path.

The current publish endpoint accepts HTTP POST requests and sends them to the router module so that interested subscribers are notified.
You can also specify forward routes in the local configuration file. When an article is published to a matching path and after the article is published
to all subscribers, it will be forwarded to the specified URL for further processing (like storage).
This way we avoid reimplementing the same type of processing twice, but we assume the final endpoint has enough throughput to handle the traffic.
You can manually publish test data, from a device that is allowed access, using any HTTP tool you like. For example:

```
curl http://airpub.mpi-sws.org/publish/path/to/article -X POST -H "Content-Type: text/plain" --data "test article content"
```

Subscribing can be done using the web-socket endpoint in airpub.
A subscribe request has the following format: "<hash> subscribe path=<path> timestamp=<timestamp>", where <hash> is the HMAC-SHA2 code of the rest of the request string and the shared secret.
A notification will come in two frames: a text one with the article header and a binary one with the article content. The article header frame has the following format: "article path=<path> content_type=<content_type> published_at=<timestamp>".

  - <path> is a string specifying the article path (must not contain spaces or equals signs).
  - <timestamp> is the number of miliseconds since the 1st of January 1970 UTC.
  - <content_type> is the type of the article supplied by the publisher.

You can use the [test/index.html](test/index.html) page to test the subscription mechanism. You need to set the secret value to the predefined value in the page ("well-known secret").
