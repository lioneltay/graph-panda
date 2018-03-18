const R = require("ramda")
const sqlFormatter = require("sql-formatter")
const knex = require("../knex")

// Turn filter AST into where string
const astToSqlWhere = ast => {
  const astToSql_ = ast => {
    if (ast.NOT) {
      const subSql = astToSql_(ast.NOT)
      return `(NOT ${subSql})`
    }

    if (ast.AND) {
      const subSql = ast.AND.map(subAst => astToSql_(subAst)).join(" AND ")
      return `(${subSql})`
    }

    if (ast.OR) {
      const subSql = ast.OR.map(subAst => astToSql_(subAst)).join(" OR ")
      return `(${subSql})`
    }

    if (ast.joins) {
      const subSql = astToSql_(ast.subClause)
      return `${ast.joins(subSql)}`
    }

    return knex.raw(ast.clause, ast.variables).toString()
  }

  return sqlFormatter.format(astToSql_(ast))
}

module.exports = {
  astToSqlWhere,
}
