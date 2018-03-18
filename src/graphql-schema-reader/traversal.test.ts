const { graphql } = require("graphql")
const R = require("ramda")
const { makeExecutableSchema } = require("graphql-tools")

// noop function for syntax highlighting
const gql = (strings, ...rest) =>
  strings.reduce((returnStr, str, index) => {
    return index === strings.length - 1
      ? returnStr + str
      : `${returnStr}${str}${rest[index].toString()}`
  }, "")

const typeDefs = gql`
  type Query {
    doStuff: String
    nugget: String
  }
`

const resolvers = {
  Query: {
    doStuff: () => "hello there!",
  },
}

const schema = makeExecutableSchema({
  resolvers,
  typeDefs,
})

test("traversal: [name here]", () => {
  const res = gql`
    a bunch of stuff
    which i ecpect ${"VALUE"} to get b ack
    exactly to same!!!
  `

  const str = `
    a bunch of stuff
    which i ecpect VALUE to get b ack
    exactly to same!!!
  `

  expect(res).toEqual(str)
})

test("traversal: basic schema", () => {
  expect.assertions(1)

  const query = gql`
    {
      doStuff
      nugget
    }
  `

  return graphql(schema, query).then(res =>
    expect(res.data.doStuff).toEqual("hello there!")
  )
})
