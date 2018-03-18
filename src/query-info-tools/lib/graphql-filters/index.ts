const R = require("ramda")
const { formatFilters } = require("./formatFilters")
const { formattedFiltersToWhereAST } = require("./formattedFiltersToWhereAST")
const { astToSqlWhere } = require("./astToSqlWhere")

const formatSql = require("sql-formatter").format

const generateWhereClause = (
  filters,
  { type, schema, tableName, debug = false } = {}
) => {
  if (debug) {
    console.log({ type, tableName })
    console.log(JSON.stringify(filters, null, 2))
  }

    if (R.isNil(filters) || R.isEmpty(filters)) {
      return undefined
    }

    if (!type || !schema || !tableName) {
      throw Error(
        "generateWhereClause: Must provide type, schema and tableName"
      )
    }

    // console.log("BEGIN")
    // Format the filters to use a more verbose syntax
    // No domain logic here
    const formattedFilters = formatFilters(filters, { type, schema })

    if (debug) {
      console.log('===== FORMATTED FILTERS =====')
      console.log(JSON.stringify(formattedFilters, null, 2))
    }

    // console.log('FORMATTED FILTERS\n', JSON.stringify(formattedFilters, null, 2))

    // Turn the filters into an sql where AST
    // ALL the domain logic is here
    // The sql stuff happens here
    const whereAST = formattedFiltersToWhereAST(formattedFilters, {
      type,
      tableName,
      schema,
    })

    if (debug) {
      console.log('===== WHERE AST =====')
      console.log(JSON.stringify(whereAST, null, 2))
    }
    // console.log('whereAST \n', JSON.stringify(whereAST, null, 2))

    // Turn the AST into a where clause string
    // No domain logic here
    const sql = astToSqlWhere(whereAST)

    if (debug) {
      console.log('===== SQL =====')
      console.log(JSON.stringify(sql, null, 2))
    }
    // console.log('sql \n', JSON.stringify(sql, null, 2))

    return formatSql(sql)
}

module.exports = {
  generateWhereClause,
}

setTimeout(() => {
  console.log("\n+++ GRAPHQL FILTER TEST +++\n")

  console.log("hello")

  console.log("\n+++++++++++++++++++++++++++")
}, 1000)
