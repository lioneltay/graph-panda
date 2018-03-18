import * as R from "ramda"
import { format } from "sql-formatter"
import { knex } from "../knex"

// Turn filter AST into where string
export const astToSqlWhere = ast => {
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

  return format(astToSql_(ast))
}
