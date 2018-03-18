var R = require('ramda');
/**
 * Mock a schema for testing purposes
 * Also serves as documentation of the shape of a graphql schema
 */
var Type = {
    name: "",
    description: "",
    astNode: {},
    extensionASTNodes: [],
    isTypeOf: undefined,
    _typeConfig: {},
    _interfaces: [],
    _fields: {},
};
var mockSchema = function (_a) {
    var _b = (_a === void 0 ? {} : _a)._typeMap, _typeMap = _b === void 0 ? {} : _b;
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
                        sqlExpr: function () { return ""; },
                        sqlColumn: "here",
                    },
                },
            },
        }),
        _implementations: {}
    };
};
module.exports = { mockSchema: mockSchema };
//# sourceMappingURL=mock-schema.js.map