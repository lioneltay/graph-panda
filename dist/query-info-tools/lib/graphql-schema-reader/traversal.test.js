var __makeTemplateObject = (this && this.__makeTemplateObject) || function (cooked, raw) {
    if (Object.defineProperty) { Object.defineProperty(cooked, "raw", { value: raw }); } else { cooked.raw = raw; }
    return cooked;
};
var graphql = require("graphql").graphql;
var R = require("ramda");
var makeExecutableSchema = require("graphql-tools").makeExecutableSchema;
// noop function for syntax highlighting
var gql = function (strings) {
    var rest = [];
    for (var _i = 1; _i < arguments.length; _i++) {
        rest[_i - 1] = arguments[_i];
    }
    return strings.reduce(function (returnStr, str, index) {
        return index === strings.length - 1
            ? returnStr + str
            : "" + returnStr + str + rest[index].toString();
    }, "");
};
var typeDefs = gql(__makeTemplateObject(["\n  type Query {\n    doStuff: String\n    nugget: String\n  }\n"], ["\n  type Query {\n    doStuff: String\n    nugget: String\n  }\n"]));
var resolvers = {
    Query: {
        doStuff: function () { return "hello there!"; },
    },
};
var schema = makeExecutableSchema({
    resolvers: resolvers,
    typeDefs: typeDefs,
});
test("traversal: [name here]", function () {
    var res = gql(__makeTemplateObject(["\n    a bunch of stuff\n    which i ecpect ", " to get b ack\n    exactly to same!!!\n  "], ["\n    a bunch of stuff\n    which i ecpect ", " to get b ack\n    exactly to same!!!\n  "]), "VALUE");
    var str = "\n    a bunch of stuff\n    which i ecpect VALUE to get b ack\n    exactly to same!!!\n  ";
    expect(res).toEqual(str);
});
test("traversal: basic schema", function () {
    expect.assertions(1);
    var query = gql(__makeTemplateObject(["\n    {\n      doStuff\n      nugget\n    }\n  "], ["\n    {\n      doStuff\n      nugget\n    }\n  "]));
    return graphql(schema, query).then(function (res) {
        return expect(res.data.doStuff).toEqual("hello there!");
    });
});
//# sourceMappingURL=traversal.test.js.map