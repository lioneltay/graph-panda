// Use the generated source maps
import "source-map-support/register"

import { makeExecutableSchema } from "graphql-tools"
import * as R from "ramda"
import { graphql } from "graphql"
import { knex } from "./lib/knex"
import * as knex from "knex"

console.log("what")
console.log("\n\n@@@")
console.log(knex.raw(`select * from "core-users"`).toString())
console.log("wheredasdfasdf")
console.log("\n\n@@@")

const typeDefs = `
  type Query {
    user: User
    hello: String
  }

  type User {
    namer: String
    age: Int
    posts: [Post]
  }

  type Post {
    content: String
    author: User
  }
`

const log = obj => console.log(JSON.stringify(obj, null, 2))

const resolvers = {
  Query: {
    user: (parent, args, context, info) => {
      return { namer: "biob" }
    },
  },

  User: {
    posts: (parent, args, context, info) => {
      return [{ content: "Cool Stuff" }, { content: "bori g stufdf" }]
    },
  },

  Post: {
    author: (parent, args, context, info) => {
      return { namer: "author dude" }
    },
  },
}

const schema = makeExecutableSchema({ typeDefs, resolvers })

const query = `
  {
    user {
      namer
      age
      posts {
        content
      }
    }
  }
`

const context = {}
const variables = {}

graphql(schema, query, context, variables).then(console.log)
