const R = require("ramda")
const { makeExecutableSchema } = require("graphql-tools")
const { annotateSchema } = require("../annotate-graphql-schema")

const mergeDeepAll = R.reduce(R.mergeDeepLeft, {})

// Model: { type: String, definitions: GraphQLSchemaAnnotation }
// createSchema = inputModels: [Model] => GraphQLSchema
export const createSchema = inputModels => {
  const typeAnnotations = mergeDeepAll(
    inputModels.map(R.prop("definitions")).filter(Boolean)
  )

  const typeDefs = inputModels
    .map(R.prop("type"))
    .filter(Boolean)
    .concat(
      `type Query {
        nothing: Int
      }

      type Mutation
      {
        nothing: Int
      }

      type Subscription {
        nothing: Int
      }`
    )

  const schema = makeExecutableSchema({ typeDefs })

  annotateSchema(schema, typeAnnotations)

  return schema
}
