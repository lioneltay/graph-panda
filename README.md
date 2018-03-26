# graphql-tekk

A collection of tools for implementing common features of a graphql server. Get everything you need by adding descriptions to your GraphQL Schema

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

### Business Data Layer (TODO)

Create data Models to manipulate data using the same GraphQL Schema

* [ ] map sqlcolumns fields to sql table fields
* [ ] for properties that depend on multiple columns, define a reverse mapping that turns the computed value into multiple parts which can be updated
  * eg firstName, lastName -> fullName bob + lee -> bob lee, bobby lee -> bobby + lee
  * obviously only if the transformation is invertible
* [ ] Support upsert queries

* [ ] should be able to check constraints before an operation, hooks?

### Pagination (TODO)

Configure pagination through the schema

### Schema Validation (TODO)

Define what constitutes a valid schema be free of worries

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

* [ ] Figure out how peerDependencies work...
  * [ ] Do peerdependencies have to be duplicated in devDependencies?

## DEMOS

* [ ] Mini server with filters
* [ ] schema reading example
