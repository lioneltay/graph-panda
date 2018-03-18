"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
// Use the generated source maps
require("source-map-support/register");
var graphql_tools_1 = require("graphql-tools");
var graphql_1 = require("graphql");
var knex_1 = require("./lib/knex");
console.log("what");
console.log("\n\n@@@");
console.log(knex_1.knex.raw("select * from \"core-users\"").toString());
console.log("wheredasdfasdf");
console.log("\n\n@@@");
var typeDefs = "\n  type Query {\n    user: User\n    hello: String\n  }\n\n  type User {\n    namer: String\n    age: Int\n    posts: [Post]\n  }\n\n  type Post {\n    content: String\n    author: User\n  }\n";
var log = function (obj) { return console.log(JSON.stringify(obj, null, 2)); };
var resolvers = {
    Query: {
        user: function (parent, args, context, info) {
            return { namer: "biob" };
        },
    },
    User: {
        posts: function (parent, args, context, info) {
            return [{ content: "Cool Stuff" }, { content: "bori g stufdf" }];
        },
    },
    Post: {
        author: function (parent, args, context, info) {
            return { namer: "author dude" };
        },
    },
};
var schema = graphql_tools_1.makeExecutableSchema({ typeDefs: typeDefs, resolvers: resolvers });
var query = "\n  {\n    user {\n      namer\n      age\n      posts {\n        content\n      }\n    }\n  }\n";
var context = {};
var variables = {};
graphql_1.graphql(schema, query, context, variables).then(console.log);
//# sourceMappingURL=index.js.map