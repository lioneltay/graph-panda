import * as R from "ramda"
import { objToArray, onlyKey, onlyValue, singleKey } from "./helpers"
import {
  readSqlBatch,
  readJunction,
  readReturnType,
} from "../graphql-schema-reader"

const sqlFilterKey = key =>
  R.contains(key, [
    "gt",
    "lt",
    "in",
    "regex",
    "like",
    "eq",
    "gte",
    "lte",
    "null",
  ])
const relationalFilterKey = key => R.contains(key, ["some", "all", "none"])
const combinationKey = key => R.contains(key, ["AND", "OR"])
const relationalField = ({ schema, type, field }) => {
  return (
    readSqlBatch({ schema, type, field }) ||
    readJunction({ schema, type, field })
  )
}

const formatOR = (orFilters, { schema, type }) => {
  const res = orFilters.OR.map(filter =>
    formatFilters(filter, { schema, type })
  ).reduce((acc, v) => {
    if (R.has("AND", v) && singleKey(v.AND)) {
      return acc.concat(v.AND)
    }

    if (R.has("OR", v)) {
      return acc.concat(v.OR)
    }

    return acc.concat(v)
  }, [])

  return { OR: res }
}

const formatAND = (andFilters, { schema, type }) => {
  const arr = andFilters.AND.map(filter =>
    formatFilters(filter, { schema, type })
  ).reduce((acc, v) => {
    if (R.has("AND", v)) {
      return acc.concat(v.AND)
    }

    if (R.has("OR", v) && singleKey(v.OR)) {
      return acc.concat(v.OR)
    }

    return acc.concat(v)
  }, [])

  return { AND: arr }
}

export const formatFilters = (filters, { schema, type }) => {
  // { property: { ... }, proerty2: { ... }, ... }
  if (!singleKey(filters)) {
    const res = formatFilters(
      {
        AND: objToArray(filters),
      },
      { schema, type }
    )
    return res
  }

  // { AND/or: [{}]}
  if (
    singleKey(filters) &&
    combinationKey(onlyKey(filters)) &&
    onlyValue(filters).length === 1
  ) {
    return formatFilters(onlyValue(filters)[0], { schema, type })
  }

  // { AND: [...] }
  if (onlyKey(filters) === "AND") {
    return formatAND(filters, { schema, type })
  }

  // { OR: [...] }
  if (onlyKey(filters) === "OR") {
    return formatOR(filters, { schema, type })
  }

  // { value: { NOT: { gt: 3 } } }
  if (singleKey(onlyValue(filters)) && onlyKey(onlyValue(filters)) === "NOT") {
    return {
      NOT: formatFilters(
        { [onlyKey(filters)]: onlyValue(onlyValue(filters)) },
        { schema, type }
      ),
    }
  }

  // { NOT: { value: { gt: 3 } } }
  if (singleKey(onlyValue(filters)) && onlyKey(filters) === "NOT") {
    return {
      NOT: formatFilters(onlyValue(filters), { schema, type }),
    }
  }

  // Relational field (has sqlBatch or junction in schema)
  if (relationalField({ schema, type, field: onlyKey(filters) })) {
    const field = onlyKey(filters) // user/post/bla
    const filterProps = onlyValue(filters) // object of filters

    if (!singleKey(filterProps)) {
      return formatFilters(
        {
          AND: objToArray(filterProps).map(val => ({ [field]: val })),
        },
        { schema, type }
      )
    }

    if (combinationKey(onlyKey(filterProps))) {
      return {
        [onlyKey(filterProps)]: onlyValue(filterProps).map(val =>
          formatFilters({ [field]: val }, { schema, type })
        ),
      }
    }

    if (["SOME", "ALL", "NONE"].includes(onlyKey(filterProps))) {
      return {
        [field]: {
          [onlyKey(filterProps)]: formatFilters(onlyValue(filterProps), {
            schema,
            type: readReturnType({ schema, type, field: field }),
          }),
        },
      }
    } else {
      return {
        [field]: formatFilters(filterProps, {
          schema,
          type: readReturnType({ schema, type, field: field }),
        }),
      }
    }
  }

  // { property: { filterType: value } }
  if (
    singleKey(onlyValue(filters)) &&
    sqlFilterKey(onlyKey(onlyValue(filters)))
  ) {
    return filters
  }

  // // { property: { relationalType: value } }
  // if (
  //   singleKey(onlyValue(filters)) &&
  //   relationalFilterKey(onlyKey(onlyValue(filters)))
  // ) {
  //   return filters
  // }

  // { property; { ... } }
  if (!combinationKey(onlyKey(filters)) && !singleKey(onlyValue(filters))) {
    return formatFilters(
      {
        AND: objToArray(onlyValue(filters)).map(clause => ({
          [onlyKey(filters)]: clause,
        })),
      },
      { schema, type }
    )
  }
}

module.exports = {
  formatFilters,
}
