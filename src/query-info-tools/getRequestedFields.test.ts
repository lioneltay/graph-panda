const { graphql } = require("graphql")
const { makeExecutableSchema } = require("graphql-tools")
const { getRequestedFields } = require("./getRequestedFields")

const typeDefs = `
  type Query {
    users: [User]
  }

  type User {
    firstName: String
    lastName: String
    email: String
    age: Int

    posts: [Post]
  }

  type Post {
    title: String
    content: String

    author: User
  }
`

test("getRequestedFields: gets requested fields on root", () => {
  expect.assertions(1)

  let result

  const resolvers = {
    Query: {
      users: (parent, args, context, info) => {
        result = getRequestedFields(info)
      },
    },
  }

  const query = `
    {
      users {
        firstName
        lastName
      }
    }
  `

  const schema = makeExecutableSchema({ typeDefs, resolvers })

  return graphql(schema, query).then(() =>
    expect(result).toEqual(["firstName", "lastName"])
  )
})

test("getRequestedFields: handles fragments on root field", () => {
  expect.assertions(1)

  let result

  const resolvers = {
    Query: {
      users: (parent, args, context, info) => {
        result = getRequestedFields(info)
      },
    },
  }

  const query = `
    {
      users {
        firstName
        lastName
        ...userFields
      }
    }

    fragment userFields on User {
      email
      age
    }
  `

  const schema = makeExecutableSchema({ typeDefs, resolvers })

  return graphql(schema, query).then(() =>
    expect(result).toEqual(["firstName", "lastName", "email", "age"])
  )
})

test("getRequestedFields: gets requested fields on sub field", () => {
  expect.assertions(1)

  let result

  const resolvers = {
    Query: {
      users: (parent, args, context, info) => {
        return [{ firstName: "bob" }]
      },
    },

    User: {
      posts: (parent, args, context, info) => {
        result = getRequestedFields(info)
      },
    },
  }

  const query = `
    {
      users {
        posts {
          title
          content
        }
      }
    }
  `

  const schema = makeExecutableSchema({ typeDefs, resolvers })

  return graphql(schema, query).then(() =>
    expect(result).toEqual(["title", "content"])
  )
})

test("getRequestedFields: handles fragments on sub field", () => {
  expect.assertions(1)

  let result

  const resolvers = {
    Query: {
      users: (parent, args, context, info) => {
        return [{ firstName: "bob" }]
      },
    },

    User: {
      posts: (parent, args, context, info) => {
        result = getRequestedFields(info)
      },
    },
  }

  const query = `
    {
      users {
        posts {
          title
          ...postFields
        }
      }
    }

    fragment postFields on Post {
      content
    }
  `

  const schema = makeExecutableSchema({ typeDefs, resolvers })

  return graphql(schema, query).then(() =>
    expect(result).toEqual(["title", "content"])
  )
})
