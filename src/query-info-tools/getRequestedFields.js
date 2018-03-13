const R = require("ramda")

const getFragment = (info, fragmentName) => {
  return info.fragments[fragmentName]
}

const getFragmentFields = (info, fragmentName) => {
  return extractFieldsFromSelections(
    info,
    getFragment(info, fragmentName).selectionSet.selections
  )
}

const extractFieldsFromSelections = (info, selections) => {
  return selections.reduce((fields, item) => {
    const { kind, name } = item
    if (kind === "Field") {
      return fields.concat(name.value)
    }

    if (kind === "FragmentSpread") {
      return fields.concat(getFragmentFields(info, name.value))
    }

    return fields
  }, [])
}

/**
 * Returns the requested fields at the given node
 */
const getRequestedFields = resolveInfo => {
  return R.uniq(
    extractFieldsFromSelections(
      resolveInfo,
      resolveInfo.fieldNodes[0].selectionSet.selections
    )
  )
}

module.exports = {
  getRequestedFields,
}
