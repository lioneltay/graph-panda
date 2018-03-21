const { annotateSchema } = require("./index")
const { graphql } = require("graphql")

const { makeExecutableSchema } = require("graphql-tools")

const typeDefs = `
  type Query {
    hello: String
  }
`

const schema = makeExecutableSchema({ typeDefs })

annotateSchema(schema, {
  Query: {
    fields: {
      hello: {
        resolve: () => "Hello World!",
      },
    },
  },
})

test("annotateSchema: ", () => {
  expect.assertions(1)

  const query = `
    {
      hello
    }
  `

  return graphql(schema, query).then(res =>
    expect(res.data.hello).toEqual("Hello World!")
  )
})
