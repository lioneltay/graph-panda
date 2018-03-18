/**
 * Given a list of Model Definitions, create the 'join-monsterised' schema
 * Model Definitions must be of the form { type: String, definitions: Object }
 * Where definitions is a join-monster annotated Object Type as used with
 *  the graphql.js reference implementation syntax
 */
const R = require("ramda")
const { makeExecutableSchema } = require("graphql-tools")
// const joinMonsterAdapter = require.main.require(
//   "../lib/join-monster-graphql-tools-adapter"
// )
const { annotateSchema } = require("../annotate-graphql-schema")

const truthyOnly = R.filter(Boolean)
const mergeDeepAll = R.reduce(R.mergeDeepLeft, {})
const mergeDeepAllTruthy = R.pipe(truthyOnly, mergeDeepAll)

const CUSTOM_PROPERTIES = ["cownugget"]

const JM_PROPERTIES = [
  "orderBy",
  "limit",
  "fields",
  "jmIgnoreTable",
  "sqlTable",
  "sqlDeps",
  "sqlJoin",
  "sqlColumn",
  "where",
  "uniqueKey",
  "sqlExpr",
  "junction",
  "sqlBatch",
].concat(CUSTOM_PROPERTIES)

const isJMProp = (value, key) => R.contains(key, JM_PROPERTIES)
const extractJMProps = R.pickBy(isJMProp)

const fieldsLens = R.lensProp("fields")

const notEmpty = R.complement(R.isEmpty)

const hasResolver = R.pipe(R.either(R.prop("resolve"), R.prop("subscribe")))

const pickResolverFields = item => {
  if (item.subscribe) {
    return R.pick(["resolve", "subscribe"], item)
  } else {
    return R.prop("resolve", item)
  }
}

const extractResolvers = R.pipe(
  R.map(R.prop("fields")),
  // R.map(R.map(R.pick(["resolve", "subscribe"]))),
  R.map(R.filter(hasResolver)),
  R.map(R.map(pickResolverFields)),
  R.filter(notEmpty)
)

const extractJMAnnotations = R.pipe(
  R.map(extractJMProps),
  R.map(R.over(fieldsLens, R.map(extractJMProps)))
)

const splitDef = def => ({
  resolvers: extractResolvers(def),
  jmAnnotations: extractJMAnnotations(def),
})

const createSchema = InputModels => {
  const Models = InputModels.map(model => {
    return R.merge(model, model.definitions ? splitDef(model.definitions) : {})
  })

  const schema = makeExecutableSchema({
    typeDefs: Models.map(R.prop("type"))
      .filter(Boolean)
      .concat(
        `type Query { nothing: Int } type Mutation { nothing: Int } type Subscription { nothing: Int }`
      ),
    resolvers: mergeDeepAllTruthy(Models.map(R.prop("resolvers"))),
  })

  // Mutates the schema
  annotateSchema(
    schema,
    mergeDeepAllTruthy(Models.map(R.prop("jmAnnotations")))
  )

  return schema
}

module.exports = {
  createSchema,
}
