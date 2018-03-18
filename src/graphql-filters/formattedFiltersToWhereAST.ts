import * as R from "ramda"

import { onlyKey, onlyValue } from "./helpers"
import {
  readSqlColumn,
  readSqlExpr,
  readJunction,
  readSqlBatch,
  readSqlTable,
  readUniqueKey,
} from "../graphql-schema-reader"

const traverseFormattedFilters = (filters, fn) => {
  const key = onlyKey(filters)
  const value = onlyValue(filters)

  if (R.contains(key, ["AND", "OR"])) {
    return { [key]: value.map(filter => traverseFormattedFilters(filter, fn)) }
  }

  return fn(filters)
}

const mapFieldName = ({ type, tableName, schema = {}, fieldName }) => {
  const col = readSqlColumn({ schema, type, field: fieldName })
  const expr = readSqlExpr({ schema, type, field: fieldName })

  return col
    ? `${tableName}.${col}`
    : expr
      ? `${R.type(expr) === "Function" ? expr(tableName) : expr}`
      : `${tableName}.${fieldName}`
}

const parseJunctionField = (clause, { type, tableName, schema }) => {
  const fieldName = onlyKey(clause)
  const filters = onlyValue(clause)
  const key = onlyKey(filters)
  const value = onlyValue(filters)

  const junction = readJunction({ schema, type, field: fieldName })

  const { sqlTable: junctionTable, sqlBatch, returnType } = junction
  const { parentKey, thisKey, sqlJoin } = sqlBatch

  const parentUniqueKey = readUniqueKey({ schema, type })
  const parentTable = readSqlTable({ schema, type })

  const thisTable = readSqlTable({ schema, type: returnType })

  const generateJoins = ({ inSet, negateWhere }) => where => `
      ${tableName}.${parentUniqueKey} ${inSet ? "IN" : "NOT IN"} (
        SELECT ${parentTable}.${parentKey} FROM ${parentTable}
          INNER JOIN ${junctionTable} ON ${junctionTable}.${thisKey} = ${parentTable}.${parentKey}
          INNER JOIN ${thisTable} ON ${sqlJoin(junctionTable, thisTable)}
        WHERE ${negateWhere ? "NOT " : ""}${where}
      )
    `

  const subClause = formattedFiltersToWhereAST(value, {
    type: returnType,
    schema,
    tableName: readSqlTable({ schema, type: returnType }),
  })

  if (key === "SOME") {
    return {
      joins: generateJoins({ inSet: true, negateWhere: false }),
      subClause,
    }
  }

  if (key === "NONE") {
    return {
      joins: generateJoins({ inSet: false, negateWhere: false }),
      subClause,
    }
  }

  if (key === "ALL") {
    return {
      joins: generateJoins({ inSet: false, negateWhere: true }),
      subClause,
    }
  }
}

const parseSqlBatchField = (clause, { type, tableName, schema }) => {
  const fieldName = onlyKey(clause)
  const filters = onlyValue(clause)
  const key = onlyKey(filters)
  const value = onlyValue(filters)

  const sqlBatch = readSqlBatch({ schema, type, field: fieldName })

  // Relational Fields

  const { returnType, thisKey, parentKey } = sqlBatch

  const parentUniqueKey = readUniqueKey({ schema, type })
  const parentTable = readSqlTable({ schema, type })

  const thisTable = readSqlTable({ schema, type: returnType })

  const generateJoins = ({ inSet, negateWhere }) => where => `
    ${tableName}.${parentUniqueKey} ${inSet ? "IN" : "NOT IN"} (
      SELECT ${parentTable}.${parentUniqueKey} FROM ${parentTable}
        INNER JOIN ${thisTable} ON ${thisTable}.${thisKey} = ${parentTable}.${parentKey}
      WHERE ${negateWhere ? "NOT " : ""}${where}
    )
  `

  const generateSubClause = filters =>
    formattedFiltersToWhereAST(filters, {
      type: returnType,
      schema,
      tableName: readSqlTable({ schema, type: returnType }),
    })

  if (key === "SOME") {
    return {
      joins: generateJoins({ inSet: true, negateWhere: false }),
      subClause: generateSubClause(value),
    }
  }

  if (key === "NONE") {
    return {
      joins: generateJoins({ inSet: false, negateWhere: false }),
      subClause: generateSubClause(value),
    }
  }

  if (key === "ALL") {
    return {
      joins: generateJoins({ inSet: false, negateWhere: true }),
      subClause: generateSubClause(value),
    }
  }

  return {
    joins: generateJoins({ inSet: true, negateWhere: false }),
    subClause: generateSubClause(filters),
  }
}

const parsePropertyField = (clause, { type, tableName, schema }) => {
  const fieldName = onlyKey(clause)
  const filters = onlyValue(clause)
  const key = onlyKey(filters)
  const value = onlyValue(filters)

  const mappedField = mapFieldName({ type, fieldName, schema, tableName })

  if (key === "null") {
    return {
      clause: `${mappedField} ${value ? "IS NULL" : "IS NOT NULL"}`,
      variables: [],
    }
  }

  if (key === "eq") {
    return {
      clause: `${mappedField} = ?`,
      variables: [value],
    }
  }

  if (key === "gt") {
    return {
      clause: `${mappedField} > ?`,
      variables: [value],
    }
  }

  if (key === "gte") {
    return {
      clause: `${mappedField} >= ?`,
      variables: [value],
    }
  }

  if (key === "lt") {
    return {
      clause: `${mappedField} < ?`,
      variables: [value],
    }
  }

  if (key === "lte") {
    return {
      clause: `${mappedField} <= ?`,
      variables: [value],
    }
  }

  if (key === "regex") {
    return {
      clause: `${mappedField} ~ ?`,
      variables: [value],
    }
  }

  if (key === "like") {
    return {
      clause: `${mappedField} LIKE ?`,
      variables: [value],
    }
  }

  if (key === "in") {
    return {
      clause: `${mappedField} in (${value.map(_ => "?").join(", ")})`,
      variables: value,
    }
  }

  throw Error(`Invalid filter key [${key}]`)
}

const parseClause = (clause, { type, tableName, schema }) => {
  const fieldName = onlyKey(clause)

  if (fieldName === "NOT") {
    return {
      NOT: formattedFiltersToWhereAST(onlyValue(clause), {
        type,
        tableName,
        schema,
      }),
    }
  }

  // Relational Fields
  if (readJunction({ schema, type, field: fieldName })) {
    return parseJunctionField(clause, { type, tableName, schema })
  }

  if (readSqlBatch({ schema, type, field: fieldName })) {
    return parseSqlBatchField(clause, { type, tableName, schema })
  }

  // Property Fields
  return parsePropertyField(clause, { type, tableName, schema })
}

export const formattedFiltersToWhereAST = (
  formattedFilters,
  { type, tableName, schema }
) => {
  return traverseFormattedFilters(formattedFilters, filter =>
    parseClause(filter, { type, tableName, schema })
  )
}
