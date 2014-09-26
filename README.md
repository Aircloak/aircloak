airpub
==========

| Branch      | Build status |
|-------------|--------------|
| develop     | [![Build Status](https://magnum.travis-ci.com/Aircloak/airpub.png?token=aFqD8qTNFV1Li4zdKtZw&branch=develop)](https://magnum.travis-ci.com/Aircloak/airpub) |

----------------------

- [What it does](#what-it-does)
- [Getting started](#getting-started)

----------------------

# What it does

Airpub is a distribution centre for anonymized metrics.

cloak-core sends results to airpub, which in turn persists the results to the database.
Analysts can also subscribe to particular queries in airpub, and get results sent directly
as they become available. This is necessary for streaming queries, but also extremely useful
for batch queries.

## Initial design sketch

XMPP has support for [pubsub](http://www.xmpp.org/extensions/xep-0060.html) and a proven track record.
I think it would be foolish to try to replicate what already exists.
[ejabberd](http://www.ejabberd.im/) seems to be the golden standard when it comes to Jabber servers, and has full pubsub support.
I suggest we start experimenting with it and see if it works and suits our needs.

### Authorization

There are two stages to authorization:

- authentication when logging onto the jabber server
- authentication when subscribing to a node

#### Server auth

ejabberd provides a plugin infrastructure where we can provide a tool that [is queried when users attempt to log in](http://www.ejabberd.im/extauth).
ejabberd is written in Erlang, and if I am not mistaken this is implemented as a port.

We can write a lightweight wrapper that checks authentications against the web to verify if a given user should have access or not.

#### Node subscription

The terminology might not be quite right, but I believe a _stream_ is called a node.
When publishing you publish to a certain node (for example `/<analyst-id>/<query-id>/...`), and when you subscribe you can attach to any point in this tree structure. For example subscribing to `/<analyst-id>` is as valid as subscribing to `/<analyst-id>/<query-id>/<bucket-name>`.

A subscriber does not receive any updates to a subscription before an owner has granted the subscriber permissions.
This means, I believe, we need to implement an XMPP user account that grants connectees access, based on what they are allowed to subscribe to.
Again this component can confer with the web.

This model is very convenient for making short lived test sessions in the web. You can run a query, and then use a web based XMPP client to connect to the server and wait for the response, and get it delivered as soon as it is ready. The XMPP protocol supports temporary id's which don't persist in the server beyond the termination of the subscription. This is very convenient for this kind of use!

##### strophe

[strophe](http://strophe.im/strophejs/) is a XMPP library used for writing JavaScript client-side XMPP based apps.
It uses BOSH to get XMPP support in the client. BOSH is a comet pulling technology which transports XMPP over HTTP. BOSH is natively supported by ejabberd.

Here is a [tutorial](http://anders.conbere.org/2011/05/03/get_xmpp_-_bosh_working_with_ejabberd_firefox_and_strophe.html)

##### exmpp

exmpp is an erlang xmpp client.
We could use it for validating user privileges when they subscribe to channels.

It is written by process-one, who are also the authors of ejabberd.

The source can be found on github: [exmpp](http://processone.github.io/exmpp/). It hasn't seen much change in recent years, but I assume that is because it is stable and works.
The [erim](https://github.com/jeanparpaillon/erim) fork might be better for us. It has rebar support and seems more actively maintained.

exmpp can also be used in the cloak itself to send results to the airpub ejabberd server.


#### Privacy

We need to ensure that no discovery mechanisms allow people to see which other analysts, and tasks, are in the system!
I am not sure how this works with ejabberd. It has to be researched.


### Post-processing

Down the line, it seems like XMPP could also allow us to do some rather nifty inline and on-demand post-processing.

We could build some additional semantics into the node naming structure.
For example if we have the subscribable path `/-/<analyst>/<query>/[<bucket>]`,
we could also support `/processed/<processor-id>/-/<analyst>/<query>/[<bucket>]`

Our XMPP-daemon would then notice that a processing path was being requested, and start a stream processor.
The stream processor would in turn itself subscribe to the equivalent `/-/` path, and output to the path that was subscribed to.
Potentially these could even be chained: `/processed/<processor-id1>/processed/<processor-id2>/-/<analyst>/<query>`, where the output
of processor 2 is fed into processor 1.

This needs some more thought, but looks like it could be very convenient for example in order to provide heatmaps.

#### Processors

The processors could be written in Lua in order to allow us to easily interchange them later on, and even allow
the analyst to write processors.

### Data persistence

In order to have anonymized results persisted to the database so they can also be shown in the web interface on [hello.aircloak.com](https://hello.aircloak.com),
we create one or multiple XMPP clients that subscribe to different nodes in the tree. They then directly persist data to Postgres as they receive it.
As a result the Rails application no longer needs to be involved in persisting data at all.

## Running locally

We need to make it so that we can run the whole setup locally for testing.
Getting ejabberd running locally should not be a problem.
We need to instruct cloak-core where to send results to though, and we also need to configure the data persistence daemon to persist the anonymous results in the right database.
This can probably be done through environmental variables with sensible defaults?

# Getting started

To be completed once we have a minimal version.
