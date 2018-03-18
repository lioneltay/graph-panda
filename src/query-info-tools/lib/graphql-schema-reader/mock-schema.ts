const R = require('ramda')

/**
 * Mock a schema for testing purposes
 * Also serves as documentation of the shape of a graphql schema
 */

const Type = {
  name: "",
  description: "",
  astNode: {},
  extensionASTNodes: [],
  isTypeOf: undefined,
  _typeConfig: {},
  _interfaces: [],
  _fields: {},
}

const mockSchema = ({_typeMap = {} } = {}) => {
  return {
    _queryType: Type,
    _mutationType: Type,
    _subscriptionType: Type,
    _directives: [],
    astNode: undefined,
    // _typeMap: { type: Type }
    _typeMap: R.merge(_typeMap, {
      SomeType: {
        _fields: {
          allYourFields: {
            someDefaults: "!",
            anyCustomFields: "df",
            sqlExpr: () => ``,
            sqlColumn: "here",
          },
        },
      },
    }),
    _implementations: {}
  }
}

module.exports = { mockSchema }