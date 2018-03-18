const R = require("ramda")

const readType = ({ schema, type }) => {
  return schema._typeMap[type]
}

const readField = ({ schema, type, field }) => {
  const resField = schema._typeMap[type]._fields[field]

  if (R.isNil(resField)) {
    throw Error(`Field: [${field}] does not exist on the schema`)
  }

  return resField
}

export const readJunction = ({ schema, type, field }) => {
  try {
    return schema._typeMap[type]._fields[field].junction
  } catch (e) {
    throw Error(`readJunction(${type}, ${field})`)
  }
}

export const readSqlBatch = ({ schema, type, field }) => {
  return readField({ schema, type, field }).sqlBatch
}

export const readSqlColumn = ({ schema, type, field }) => {
  return schema._typeMap[type]._fields[field].sqlColumn
}

export const readSqlExpr = ({ schema, type, field }) => {
  return schema._typeMap[type]._fields[field].sqlExpr
}

export const readSqlTable = ({ schema, type }) => {
  return schema._typeMap[type]._typeConfig.sqlTable
}

export const readUniqueKey = ({ schema, type }) => {
  return schema._typeMap[type]._typeConfig.uniqueKey
}

export const readReturnType = ({ schema, type, field }) => {
  let returnType
  const sqlBatch = readSqlBatch({ schema, type, field })

  if (sqlBatch) {
    returnType = sqlBatch.returnType
  }

  const junction = readJunction({ schema, type, field })

  if (junction) {
    returnType = junction.returnType
  }

  if (R.isNil(returnType)) {
    throw Error(
      `Field [${field}] of Type [${type}] does not have returnType specified`
    )
  }

  if (R.isNil(readType({ type: returnType, schema }))) {
    throw Error(`returnType: [${returnType}] does not exist on the schema`)
  }

  return returnType
}

const traverseSchema = ({ schema }) => {
  // do stuff
}

const traverseSchemaWithContext = ({ schema }) => {
  // do stuff
}

const wrapResolvers = ({ schema }) => {
  // do stuff
}
