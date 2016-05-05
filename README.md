# Introducing Brahman

_A universe of backends for Om Next & other CQRS systems._

## What is Brahman and what can it do for me?

Brahman aims to solve the problem of connecting client-driven CQRS
systems like Om Next with server side databases, event sourcing and
business logic. It offers a mostly declarative way of writing
colocated specifications for data models, data validation, commands,
command validation and command authorization, and a simple interface
to query/process these models and commands.

### Motivation

With Om Next, most of the client side complexity of querying and
mutating data goes away. However, there is no equivalent story for an
equally tidy, declarative, colocated, extensible way of handling
queries and mutations on the server side. Brahman aims to provide one.

We realized that there are all these nice libraries and tools out there
for writing backends - Sente, Buddy / Friend, Datomic, Bouncer /
Prismatic Schema, Onyx, Kafka to name just a few - but integrating these
with Om Next queries and mutations is not straight forward enough.

Also, even though Datomic performs data-level validation and Buddy /
Friend provide general authnz functionality, our feeling was that almost
everyone will be looking for a way to perform validation that yields
human-readable errors and authorization that operates on commands
rather than e.g. HTTP requests, typically from within an Om Next server
side parser or similar.

Which leads us to Brahman's features:

### Features

* Models
  - Define models as materialized views on top of _any_ number
    of databases
  - Fetch and query model data using _any_ query language
  - Specify model version (in any format), schemas (in any format),
    validation rules (in any format) and data stores (of any kind)
    all in one place (colocation), in a declarative way, as data
  - Extend data with attributes derived from any of the databases
    on the fly
* Commands
  - Define commands in the system, including their versions, validation
    and authorization rules, in one place (colocation), declaratively,
    as data
  - Validate commands and command parameters
      * Using model schemas and validations
      * Using database queries
      * Using arbitrary functions / predicates
  - Authorize commands
      * Using the command environment (can include arbitrary information
        about client, authenticated user, database etc.)
      * Using database queries
      * Using arbitrary functions / predicates
      * Producing validation and authorization errors that are readable
        for humans and machines at the same time
  - Verify that all commands used in the system are validated
    and authorized

## How does it work?

TODO

## License

Brahman is (C) 2016 Workflo, Inc. & Jannis Pohlmann.

It is licensed under GNU LGPL 2.1. For more details see LICENSE.txt.
