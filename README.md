# graphql-tekk

A collection of tools for implementing common features of a graphql server.

## Features/Modules

### SQL generation (TODO)

Automatically generate sql queries to fetch the required data. Configuration is done through adding custom properties to the graphql schema

### Batching (TODO)

Coordinating SQL Queries

### Query Filters (TODO)

Implement filter arguments by simply describing the graphql types that should have filters

### Higher Order Resolvers (TODO)

Allow the wrapping of resolvers. Behaviour of the wrapper resolver can be configured through properties of the schema

### Schema Annotations (TODO)

Add any custom properties to an existing graphql schema

## NOTES

* make the functionality, split into modules later

## TODOS

### Features

* [ ] move graphql-filters into this package
  * [ ] Clean up code
  * [ ] Use typescript to clean up logic
* [ ] move graphql-schema-reader into this package
* [ ] move graphql-schema-annotations into this package
* [ ] graphql-higher-order-resolver
* [ ] implement similar functionality as join-monster

### Dev

* [x] add typescript
* [x] create github repo
* [x] publish to npm
* [x] figure out how to publish only the compiled src (npmignore)
* [ ] use nodejs scripts as start script
  * [ ] fix the double initial compilation

## DEMOS

* [ ] Mini server with filters
* [ ] schema reading example
