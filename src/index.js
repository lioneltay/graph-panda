const { makeExecutableSchema } = require("graphql-tools")
const R = require("ramda")
const { graphql } = require("graphql")

// const { getSubFields } = require("lib/query-info")

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
