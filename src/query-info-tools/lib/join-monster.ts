const joinMonster = require("join-monster").default
const R = require("ramda")
const knex = require.main.require("../common/functions/knex")
const formatSql = require("sql-formatter").format
const { generateWhereClause } = require.main.require("../lib/graphql-filters")

const debug = true

const defaultDBCall = sql => knex.raw(sql).then(R.prop("rows"))

const generateContext = () => ({
  knex,
  schema: require.main.require("../graphql-schema"),
  formatSql,
  generateWhereClause: (filters, options) =>
    generateWhereClause(
      filters,
      R.merge(options, {
        type: options.type,
        tableName: options.tableName,
        schema: require.main.require("../graphql-schema"),
      })
    ),
})

const jm = (resolveInfo, context = {}, dbCall = defaultDBCall, options) =>
  joinMonster(
    resolveInfo,
    R.merge(generateContext(), context),
    sql => {
      const result = dbCall(sql)

      if (debug) {
        console.log("\n======================")
        console.log(sql)
        console.log("\n======================")
        result.then(rows => {
          console.log(`Join-Monster Query rows: ${rows.length}`)
          // console.log(rows)
        })
      }

      return result
    },
    R.merge({ dialect: "pg" }, options)
  )

jm.infoOnly = (root, args, context, info) => jm(info)

module.exports = jm
