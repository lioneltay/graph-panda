var graphql = require("graphql").graphql;
var makeExecutableSchema = require("graphql-tools").makeExecutableSchema;
var getRequestedFields = require("./getRequestedFields").getRequestedFields;
var typeDefs = "\n  type Query {\n    users: [User]\n  }\n\n  type User {\n    firstName: String\n    lastName: String\n    email: String\n    age: Int\n\n    posts: [Post]\n  }\n\n  type Post {\n    title: String\n    content: String\n\n    author: User\n  }\n";
test("getRequestedFields: gets requested fields on root", function () {
    expect.assertions(1);
    var result;
    var resolvers = {
        Query: {
            users: function (parent, args, context, info) {
                result = getRequestedFields(info);
            },
        },
    };
    var query = "\n    {\n      users {\n        firstName\n        lastName\n      }\n    }\n  ";
    var schema = makeExecutableSchema({ typeDefs: typeDefs, resolvers: resolvers });
    return graphql(schema, query).then(function () {
        return expect(result).toEqual(["firstName", "lastName"]);
    });
});
test("getRequestedFields: handles fragments on root field", function () {
    expect.assertions(1);
    var result;
    var resolvers = {
        Query: {
            users: function (parent, args, context, info) {
                result = getRequestedFields(info);
            },
        },
    };
    var query = "\n    {\n      users {\n        firstName\n        lastName\n        ...userFields\n      }\n    }\n\n    fragment userFields on User {\n      email\n      age\n    }\n  ";
    var schema = makeExecutableSchema({ typeDefs: typeDefs, resolvers: resolvers });
    return graphql(schema, query).then(function () {
        return expect(result).toEqual(["firstName", "lastName", "email", "age"]);
    });
});
test("getRequestedFields: gets requested fields on sub field", function () {
    expect.assertions(1);
    var result;
    var resolvers = {
        Query: {
            users: function (parent, args, context, info) {
                return [{ firstName: "bob" }];
            },
        },
        User: {
            posts: function (parent, args, context, info) {
                result = getRequestedFields(info);
            },
        },
    };
    var query = "\n    {\n      users {\n        posts {\n          title\n          content\n        }\n      }\n    }\n  ";
    var schema = makeExecutableSchema({ typeDefs: typeDefs, resolvers: resolvers });
    return graphql(schema, query).then(function () {
        return expect(result).toEqual(["title", "content"]);
    });
});
test("getRequestedFields: handles fragments on sub field", function () {
    expect.assertions(1);
    var result;
    var resolvers = {
        Query: {
            users: function (parent, args, context, info) {
                return [{ firstName: "bob" }];
            },
        },
        User: {
            posts: function (parent, args, context, info) {
                result = getRequestedFields(info);
            },
        },
    };
    var query = "\n    {\n      users {\n        posts {\n          title\n          ...postFields\n        }\n      }\n    }\n\n    fragment postFields on Post {\n      content\n    }\n  ";
    var schema = makeExecutableSchema({ typeDefs: typeDefs, resolvers: resolvers });
    return graphql(schema, query).then(function () {
        return expect(result).toEqual(["title", "content"]);
    });
});
//# sourceMappingURL=getRequestedFields.test.js.map