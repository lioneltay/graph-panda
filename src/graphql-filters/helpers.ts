import * as R from "ramda"

export const splitSingleProperty = object => {
  const key = R.head(R.keys(object))
  const value = object[key]
  return { key, value }
}

export const singleKey = object => R.keys(object).length === 1

export const onlyKey = object => {
  if (R.keys(object).length !== 1) {
    throw Error(
      `onlyKey used on object with keys.length (${R.keys(object).length}) !== 1
        ${JSON.stringify(object, null, 2)}
      `
    )
  }

  return splitSingleProperty(object).key
}

export const onlyValue = object => {
  if (R.values(object).length !== 1) {
    throw Error("onlyValue used on object with values.length !== 1")
  }

  return splitSingleProperty(object).value
}

export const objToArray = R.pipe(R.toPairs, R.map(([k, v]) => ({ [k]: v })))
